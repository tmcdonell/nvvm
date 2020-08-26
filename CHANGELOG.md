# Revision history for nvvm

Notable changes to the project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/).

## [0.10.0.0] - 2020-08-26
### Added
  * Support for Cabal-3
  * `nvvmDeviceLibraryPath`

## [0.9.0.0] - 2018-10-02
### Fixed
  * Build fix for ghc-8.6

### Added
  * `addModuleLazy` and `addModuleLazyFromPtr` from CUDA-10.0

### Changed
  * Replace uses of `String` with `ShortByteString`

## [0.8.0.3] - 2018-03-12
### Fixed
  * Build fix for Cabal-2.2 (ghc-8.4)

## [0.8.0.2] - 2018-01-05
### Fixed
  * Fix profiling build

## [0.8.0.1] - 2017-11-15
### Fixed
  * Fix nvvm library path on windows ([#2])

## [0.8.0.0] - 2017-08-24
### Changed
  * Build setup improvements

## [0.7.5.2] - 2017-04-10
### Added
  * Add support for older c2hs versions

## [0.7.5.1] - 2016-11-08
### Added
  * Add support for Cabal-1.22

## [0.7.5.0] - 2016-10-08
  * First version. Released on an unsuspecting world.


[0.10.0.0]:     https://github.com/tmcdonell/nvvm/compare/v0.9.0.0...v0.10.0.0
[0.9.0.0]:      https://github.com/tmcdonell/nvvm/compare/v0.8.0.3...v0.9.0.0
[0.8.0.3]:      https://github.com/tmcdonell/nvvm/compare/v0.8.0.2...v0.8.0.3
[0.8.0.2]:      https://github.com/tmcdonell/nvvm/compare/v0.8.0.1...v0.8.0.2
[0.8.0.1]:      https://github.com/tmcdonell/nvvm/compare/v0.8.0.0...v0.8.0.1
[0.8.0.0]:      https://github.com/tmcdonell/nvvm/compare/v0.7.5.2...v0.8.0.0
[0.7.5.2]:      https://github.com/tmcdonell/nvvm/compare/0.7.5.1...v0.7.5.2
[0.7.5.1]:      https://github.com/tmcdonell/nvvm/compare/0.7.5.0...0.7.5.1
[0.7.5.0]:      https://github.com/tmcdonell/nvvm/compare/953f6c0b99b8d667a8e261722a8daeeaba162435...0.7.5.0

[#2]:           https://github.com/tmcdonell/nvvm/pull/2

