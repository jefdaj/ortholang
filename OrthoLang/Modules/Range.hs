{-# LANGUAGE QuasiQuotes #-}

module OrthoLang.Modules.Range
  where

import OrthoLang.Types
import OrthoLang.Interpreter

olModule :: Module
olModule = Module
  { mName = "Range"
  , mDesc = "Generate ranges of numbers"
  , mTypes = [num]
  , mGroups = []
  , mEncodings = []
  , mFunctions =
    [ rangeIntegers
    , rangeAdd
    , rangeLength
    , rangeMultiply
    , rangeExponent
    ]
  }

rangeIntegers :: Function
rangeIntegers = newFnS2
  "range_integers"
  (Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_integers.R"
  []
  id

rangeAdd :: Function
rangeAdd = newFnS3
  "range_add"
  (Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_add.R"
  []
  id

rangeLength :: Function
rangeLength = newFnS3
  "range_length"
  (Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_length.R"
  []
  id

rangeMultiply :: Function
rangeMultiply = newFnS3
  "range_multiply"
  (Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_multiply.R"
  []
  id

rangeExponent :: Function
rangeExponent = newFnS4
  "range_exponent"
  (Exactly num, Exactly num, Exactly num, Exactly num)
  (ListSigs $ Exactly num)
  "range_exponent.R"
  []
  id
