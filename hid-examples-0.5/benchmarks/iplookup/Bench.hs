{-# LANGUAGE StandaloneDeriving, DeriveGeneric, DeriveAnyClass #-}
import Criterion.Main

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Data.Maybe (fromJust)

import ParseIP
import IPTypes
import LookupIP
import qualified FastLookup as FL

import Paths_hid_examples (getDataFileName)

deriving instance Generic IP
deriving instance Generic IPRange
deriving instance Generic IPRangeDB
deriving instance NFData IP
deriving instance NFData IPRange
deriving instance NFData IPRangeDB

readIPRDBFile fname = getDataFileName (ipBenchDir ++ fname)
                      >>= readFile
  where
    ipBenchDir = "data/benchmarks/iplookup/"

iptexts = ["0.0.0.1", "192.168.1.1", "17.0.32.2",
           "255.255.252.41", "255.255.252.42"]

ips = map (\s -> (s, fromJust $ parseIP s)) iptexts

iprdb = parseValidIPRanges <$> readIPRDBFile "3.iprs"

main = defaultMain [
    bgroup "buildIP" [
      let theip = [17,0,32,2] in
      bgroup "single" [
        bench "default" $ nf buildIP theip
      , bench "foldr" $ nf buildIP_foldr theip
      , bench "foldl" $ nf buildIP_foldl theip
      , bench "foldl-shl" $ nf buildIP_foldl_shl theip
      ]

    , let ipcomps = [[0,0,0,1], [192,168,1,1], [17,0,32,2],
                     [255,255,252,41], [255,255,252,41]] in
      bgroup "several" [
       bench "default" $ nf (map buildIP) ipcomps
      , bench "foldr" $ nf (map buildIP_foldr) ipcomps
      , bench "foldl" $ nf (map buildIP_foldl) ipcomps
      , bench "foldl-shl" $ nf (map buildIP_foldl_shl) ipcomps
      ]
    ]
    
  , bench "parseIP" $ nf (map parseIP) iptexts
  , bgroup "parseIP" [
      bench "monadic" $ nf (map parseIPMonadic) iptexts
    , bench "iterative" $ nf (map parseIPIter) iptexts
    , bench "iterative-strict" $ nf (map parseIPIterStrict) iptexts
    ]

  , let rangeFiles = [("small", "1.iprs"),
                      ("middle-sized", "2.iprs"),
                      ("large", "3.iprs")] in
    bgroup "ranges" [
      bgroup "read" $
        map (\(desc, fname) ->
              bench desc $ nfIO (readIPRDBFile fname))
            rangeFiles
    , bgroup "parse" $
        map (\(desc, fname) ->
              env (readIPRDBFile fname) $
                \iprdbf -> bench desc $ nf parseIPRange iprdbf)
            rangeFiles
    ]

  , env iprdb $ \ iprdb ->
      bgroup "lookupIP" [
        bgroup "single" $
          map (\ (textip, ip) ->
                 bench textip $
                   whnf (lookupIP iprdb) ip) ips
      , bench "several" $ nf (map (lookupIP iprdb)) $ map snd ips
      ]

  , env iprdb $ \ iprdb -> let fiprdb = FL.fromIPRangeDB iprdb in
      bgroup "lookupIP (fast)" [
        bgroup "single" $
          map (\ (textip, ip) ->
                 bench textip $
                   whnf (FL.lookupIP fiprdb) ip) ips
      , bench "several" $ nf (map (FL.lookupIP fiprdb)) $ map snd ips
      ]
  ]
