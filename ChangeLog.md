# Changelog for cobot-io

## [Unreleased]

## [0.1.2.3] - 2019-12-12
### Fixed
- Fixes for .mae pasrser.
- Fixes for instance of `StructureModels` for `Mae`.

## [0.1.2.2] - 2019-11-27
### Added
- Instance of `StructureModels` for `Mae`.
### Fixed
- Fix for .mae pasrser.

## [0.1.2.1] - 2019-11-25
### Added
- Parser for `MAE`.

## [0.1.2.0] - 2019-09-03
### Added
- Parser for `FASTA`.
- Writer for `FASTA`.

## [0.1.1.1] - 2019-06-05
### Changed
- `length` on `Sequence` now works in O(1).

## [0.1.1.0] - 2019-05-13
### Added
- New version of module Bio.Sequence hat introduces `IsSequence` type class and `Sequence` datatype.
- Type `GenBankSequence` describing structure of .gb file.
- Parser for `GenBankSequence`.
- Writer for `GenBankSequence`.
### Changed
- ABI cleaner and decoder now work with type `ABIRaw`.

## [0.1.0.1] - 2019-02-28
### Added
- Field `chemCompType` in `Residue`.
### Fixed
- `codec10`.
