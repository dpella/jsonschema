# Changelog

## 0.2.0.1
- Widen dependency bounds to support GHC 9.12.2.

## 0.2.0
- Move the public API to `Data.JSON.JSONSchema` and supporting machinery to `Data.JSON.ToJSONSchema`.
- Tighten derived record/sum schemas to emit `required` arrays and discriminator requirements.
- Fix numeric validation to operate on exact `Scientific` values.
- Add regression coverage for large integers and high-precision decimals.

## 0.1.0
- Initial release with JSON Schema 2020-12 generation and validation support.
