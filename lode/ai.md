# AI Agent Guidelines for Lode Project

This document provides guidance for AI agents on how to properly make changes and additions to the Lode project codebase.

## Initial Context Gathering

1. Read all files in the `lode/` directory, especially:
   - `lode/overview.md` for project architecture and goals
   - Any other documentation files for specific components

## Documentation Requirements

### Haddock Documentation
All top-level functions must include Haddock documentation comments that specify:
- Function purpose
- Parameter descriptions
- Return value description
- Any side effects or important notes
- Examples where helpful

Example:
```haskell
-- | Converts a raw byte string into an EventsPage structure
-- 
-- @param rawBytes The input byte string to parse
-- @returns Either an error describing why parsing failed, or the parsed EventsPage
-- 
-- The byte string must be exactly 8192 bytes and follow the EventsPage format:
-- * First 2 bytes: reserve pointer
-- * Following bytes: event pointers and data
fromByteString :: ByteString -> Either PageReadError EventsPage
```

## Testing Requirements

Every new top-level function must have corresponding test cases that:
1. Test normal/expected usage
2. Test edge cases
3. Test error conditions
4. Use descriptive test names that explain the scenario

Example:
```haskell
describe "fromByteString" $ do
  it "should accept valid page data" $ do
    -- Test code
  it "should reject pages of invalid size" $ do
    -- Test code
  it "should reject invalid reserve pointer" $ do
    -- Test code
```

## Module Management

When modules are created, renamed, moved or deleted:

1. Update the `other-modules` section in both the library and test-suite sections of `new-project.cabal`
2. Maintain alphabetical ordering of modules
3. Ensure module hierarchy matches directory structure

Example cabal update:
```cabal
other-modules:    Web.Convent.Storage.EventStore
                , Web.Convent.Storage.EventsPage
                , Web.Convent.Storage.IndexPage
                , Web.Convent.Util.NewModule
```

## Documentation Updates

When making changes that affect project architecture or functionality:

1. Update `lode/overview.md` to reflect:
   - New components
   - Changed behaviors
   - Modified architectures
   - Added features
2. Keep the "Currently Implemented" and "Planned Components" sections accurate
3. Update any affected diagrams or examples

## Code Style

1. Follow existing code patterns in the module being modified
2. Use meaningful variable names
3. Keep functions focused and single-purpose
4. Document complex algorithms or non-obvious decisions
5. Use appropriate type signatures

## Error Handling

1. Use appropriate error types (`EventPageError`, `IndexPageError`, etc.)
2. Add new error variants when needed
3. Document error conditions in Haddock comments
4. Include error case tests

Remember: The goal is to maintain consistency and clarity while extending the codebase in a way that aligns with the project's messaging platform objectives.