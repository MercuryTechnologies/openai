---
name: hackage-upload
description: Upload Haskell packages to Hackage. Use when the user asks to publish, release, or upload to Hackage.
---

# Hackage Upload

## Instructions

When the user asks to publish or upload to Hackage:

1. **Do not continue until release metadata is updated**:
   - `openai.cabal` version has been bumped to the new release version
   - `CHANGELOG.md` has a top entry for that same version
   - Verify with:
     ```bash
     grep "^version:" openai.cabal
     sed -n '1,20p' CHANGELOG.md
     ```

2. **Get the current version** from `openai.cabal`:
   ```bash
   grep "^version:" openai.cabal
   ```

3. **Create the source distribution**:
   ```bash
   cabal sdist
   ```

4. **Upload to Hackage** (replace X.Y.Z with actual version):
   ```bash
   cabal upload --publish dist-newstyle/sdist/openai-X.Y.Z.tar.gz
   ```

## Pre-release checklist

Before uploading, ensure:
- Version in `openai.cabal` has been bumped to the release version
- `CHANGELOG.md` has a top entry for that exact same version
- All tests pass (`cabal test`)
- Code compiles without warnings (`cabal build`)

## Notes

- The `--publish` flag publishes immediately. Without it, the package is uploaded as a candidate.
- You must have Hackage credentials configured (typically in `~/.cabal/config`).
- First-time uploads of a package require manual approval on Hackage.
