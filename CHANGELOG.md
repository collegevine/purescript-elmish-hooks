# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.10.1

### Changed

- upgraded to PureScript 0.15.10 (allows visible type applications).
- `useState`, `useEffect`, and `useRef` now allow their type arguments to be visibly applied.

## 0.9.1

### Changed

- upgraded to Elmish 0.9.1

## 0.7.0

### Changed

- **Breaking**: Renamed `withHooks` to `component`.

## 0.6.1

### Fixed

- Fixed `useRef` docs.

## 0.6.0

### Added

- Added `useRef` hook.

## 0.5.3

### Added

- `useEffect'` is now reexported from `Elmish.Hooks` for easy access.
