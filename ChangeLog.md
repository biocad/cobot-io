# Changelog for cobot-io

## [Unreleased]

## [0.1.5.3] - 2023-12-08
- Update tests and dependencies.

## [0.1.5.2] - 2023-11-09
- Add Ord for Fasta and related types.

## [0.1.5.1] - 2023-01-17
- Update dependencies for Haskell LTS-20.4.

## [0.1.5.0] - 2022-09-30
### Changed
- Update FASTA parser to megaparsec.

## [0.1.4.4] - 2022-06-02
### Changed
- Update more dependencies;
- Fix new warnings in GHC-9.

## [0.1.4.3] - 2022-03-23
### Changed
- Support GHC-9.2 packages in dependencies.

## [0.1.4.2] - 2021-10-14
### Changed
- More types of multiline properties are supported.

## [0.1.4.1] - 2021-10-07
### Changed
- CI fix

## [0.1.4.0] - 2021-09-27
### Changed
- Redesigned the Range type to reflect all possible cases.
- Switched to Megaparsec.

## [0.1.3.25] - 2021-09-15
### Added
- Pedantic build and CI

## [0.1.3.24] - 2021-09-10
### Changed
- GB parser made more verbose.

## [0.1.3.23] - 2021-07-06
### Added
Added ASN hydrogen names sometimes set by Scho

## [0.1.3.22] - 2021-07-06
### Changed
- `*` -> `Type` for GHC-9.

## [0.1.3.21] - 2021-07-03
### Changed
- Update dependency versions.

## [0.1.3.20] - 2021-06-04
### Changed 
- YLAB2-629: Fasta parser is now able to parse empty lines in the beginning. 

## [0.1.3.19] - 2021-04-30
### Changed 
- Exports and instances for Biosset. 

## [0.1.3.18] - 2021-03-09
### Fixed
- Lowercase insertion code parsing in PDB.

## [0.1.3.17] - 2021-02-18
### Fixed
- PDB parsing.

## [0.1.3.16] - 2021-02-16
### Fixed
- Bad comment in GB parser.

## [0.1.3.15] - 2021-02-16
### Fixed
- Unknown fields in GB.

## [0.1.3.14] - 2021-02-10
### Changed
- Added ability to parse modifications in Fasta.

## [0.1.3.13] - 2021-01-02
### Changed
- Allow `QuickCheck-2.14`.

## [0.1.3.12] - 2020-11-11
### Fixed
- PDB parser error: it didn't account for segment identifier field.
- Bond restoring in PDB for non-standard residue names.

## [0.1.3.11] - 2020-11-08
### Changed
- Allow `linear-0.21`.

## [0.1.3.10] - 2020-11-06
### Fixed
- `SOURCE` and `ORIGIN` in GB parser.

## [0.1.3.9] - 2020-10-27
### Fixed
- FASTA parser can now parse empty lines with spaces. 

## [0.1.3.8] - 2020-10-22
### Fixed
- A couple of issues, that caused parametrization failures, fixed in bond restoring for PDB format.

## [0.1.3.7] - 2020-10-14
### Added
- Generic fasta parser.

## [0.1.3.6] - 2020-07-14
### Added
- Convertation from `Model`s to `PDB`.
- Writer for `PDB`.
- `renameChains` function that renames chains in a model.

## [0.1.3.5] - 2020-05-26
### Fixed
- Correctly clean `BasecalledSequenceWithRawData`, including inner quality.

## [0.1.3.4] - 2020-05-14
### Added
- `instance Cleanable BasecalledSequenceWithRawData`.

## [0.1.3.3] - 2020-05-07
### Added
- Type and decoder for `ab1` with raw channel data and peak locations.
### Fixed
- Use supplied thresholds instead of default ones in the implementation of `cleanWith`;
- Do not run `IO` in `hspec` `Spec` monad, use `beforeAll`.

## [0.1.3.2] - 2020-04-15
### Changed
- Resolver version up.
- Moving from `less-wrong` to `biocad`.

## [0.1.3.1] - 2020-04-02
### Fixed
- Reading of insertion code for residues in MAE.

## [0.1.3.0] - 2020-03-27
### Added
- Residue index in `Structure`.
- Atom input index in `Structure`.
- Bond restoring for PDB.
- Tests for PDB -> Model conversion.
### Changed
- GlobalID now 0-based in mae, PDB, and MMTF.
### Fixed
- A lot of things.

## [0.1.2.10] - 2020-03-27
### Added
- Lenses for `Structure`.

## [0.1.2.9] - 2020-03-12
### Added
- Function `filterAtomsOfModel` to filter atoms of model by the given predicate.
### Fixed
- Grouping by residues when converting `Mae` to `Model`.

## [0.1.2.8] - 2020-03-12
### Added
- `instance Traversable (Sequence mk w)`.

## [0.1.2.7] - 2020-02-11
### Changed
- Support GHC-8.8.

## [0.1.2.6] - 2019-12-12
### Fixed
- Fixes for instance of `StructureModels` for `Mae` when working with structures without explicit chain names.

## [0.1.2.5] - 2019-12-24
### Fixed
- Possibility to have spaces in Fasta sequences.

## [0.1.2.4] - 2019-12-23
### Added
- Preprocessing for pdb-files.
- Pdb parser.

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
