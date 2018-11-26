module ShortCut.Modules.MMSeqs
  where

import ShortCut.Core.Types

cutModule :: CutModule
cutModule = CutModule
  { mName = "MMSeqs"
  , mDesc = "Many-against-many sequence searching: ultra fast and sensitive search and clustering suite."
  , mTypes = []
  , mFunctions =
      [ 
      -- TODO mmseqs_createdb : fa -> mms
      -- TODO mmseqs_search_all : fa.list mms -> mmr (this is mmseqs search)
      -- TODO mmseqs_search : fa mms -> mmr (this is mmseqs search with a singleton list)
      ]
  }

-- TODO mms type for mmseqs subject (or sequence?) db?
-- TODO mmh type for mmseqs result/hits db?
