# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
This project adheres (loosely) to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [A successful Git branching model](https://nvie.com/posts/a-successful-git-branching-model/).

## [unreleased]

## [0.9.4] - 2020-01-22
### Changed
- Rename ShortCut to OrthoLang
- Refactor cabal and nix files so ortholang-demo can auto-update docs
- OrthoLangFunctions can have multiple fNames now (only binary operators use them)

### Added:
- Some BLAST docs
- Some example scripts

## [0.9.3] - 2020-01-02
### Added
- Docker image (nix expression, build script, docker hub repo)

### Fixed
- Specified linux dependencies in nix files
- Patch shebangs in Orthofinder scripts
- Stack repl finds nix-provided zlib now
- Venn Diagram list dependencies
- Provide Fontconfig path to R scripts

## [0.9.2] - 2019-12-10
### Changed
- Friendlier install script

## [0.9.1] - 2019-12-06
### Fixed
- install.sh should work now
- Upload custom release archives because git/github fails on submodules
- Improved UTF-8 locale handling

## [0.9.0] - 2019-12-05
### Changed
- Move development to `develop` branch; `master` will be for releases
- Start using semantic versioning

[unreleased]: https://github.com/jefdaj/ortholang/compare/v0.9.4...HEAD
[0.9.4]: https://github.com/jefdaj/ortholang/releases/tag/v0.9.4
[0.9.3]: https://github.com/jefdaj/ortholang/releases/tag/v0.9.3
[0.9.2]: https://github.com/jefdaj/ortholang/releases/tag/v0.9.2
[0.9.1]: https://github.com/jefdaj/ortholang/releases/tag/v0.9.1
[0.9.0]: https://github.com/jefdaj/ortholang/releases/tag/v0.9.0
