## v1.6-00 – 2025-08-15

- Documentation  
  Fixed missing “Not run” blocks in all `@examples`: replaced stray  
  `## Not run:` / `## End(Not run)` markers with proper `\dontrun{}` wrappers.

- Bug fixes  
  Addressed several small glitches in data handling and plotting functions.

- New feature  
  Added `hb_ensemble()` to create ensemble suitability maps via:
  - majority vote  
  - weighted majority vote  
  - unweighted averaging (mean)  
  - weighted averaging 
