# Changelog

## [0.2.0] - 2026-08-30

* Update to lean-json v0.2.1 and Lean v4.33.1.
* Refuse a `_meta` that cannot be read rather than reading it as one that was never sent; `requestMeta`, `metaString` and `resolveVersion` change shape.
* Answer `500` when a response header cannot be built rather than dropping the header; `headersOf` returns an `Option`.

## [0.1.3] - 2026-08-29

Add `bearer?`, which reads the credential from an `Authorization` header.

## [0.1.2] - 2026-08-24

Pin lean-json.

## [0.1.1] - 2026-08-23

Fix bug introduced by Mac case insensitive file system.

## [0.1.0] - 2026-08-23

Initial release
