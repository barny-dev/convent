# Convent

Convent is an event-based messaging platform built with Haskell. It implements an efficient binary storage format for chat events with indexing capabilities for fast message retrieval.

## Project Status

This project is in active development. The core storage layer is implemented and tested.

### Completed Features

- **Event Storage System**
  - Fixed-size pages (8192 bytes) for efficient storage
  - Binary event format with type ID prefixes
  - Space management and event retrieval
  
- **Index Pages**
  - Fast event lookup by offset
  - Index entry management
  - Page validation

- **Event Types**
  - `ParticipantJoinedEvent` (0x01) - Records when a participant joins
  - `ParticipantLeftEvent` (0x02) - Records when a participant leaves  
  - `MessageSubmittedEvent` (0x03) - Records chat messages

- **File I/O Operations**
  - Page-level read/write operations
  - Error handling and validation

### Planned Features

- HTTP API for chat interactions
- Chat management system
- Directory structure for chat storage
- Client interface
- Additional event types

## Building

Requires GHC 9.14+ and Cabal 3.10+.

```bash
cabal update
cabal build
```

## Testing

Run the test suite with:

```bash
cabal test
```

All 42 tests should pass, covering:
- EventStore operations
- EventsPage format and operations
- IndexPage structure and validation
- FilePage I/O operations
- Event encoding/decoding
- ChatFlow integration tests (end-to-end event flow scenarios)

## Running

```bash
cabal run convent-exe
```

## Project Structure

```
convent/
├── app/                    # Executable source
│   └── Main.hs
├── lib/                    # Library source
│   └── Web/
│       └── Convent/
│           ├── Error.hs
│           ├── Events/     # Event type definitions
│           │   ├── MessageSubmittedEvent.hs
│           │   ├── ParticipantJoinedEvent.hs
│           │   └── ParticipantLeftEvent.hs
│           ├── Storage/    # Storage layer
│           │   ├── ChatStore.hs
│           │   ├── EventsPage.hs
│           │   ├── EventStore.hs
│           │   ├── FilePage.hs
│           │   ├── IndexPage.hs
│           │   └── PageOps.hs
│           └── Util/
│               └── ByteString.hs
├── test/                   # Test suite
│   ├── Spec.hs
│   └── Web/Convent/...
├── lode/                   # Project documentation
│   ├── ai.md               # AI agent guidelines
│   ├── event-storage.md    # Storage format specification
│   ├── events.md           # Event format specification
│   └── overview.md         # Project overview
├── convent.cabal           # Package configuration
├── cabal.project           # Build configuration
├── LICENSE                 # MIT License
└── CHANGELOG.md            # Version history
```

## Documentation

Detailed documentation is available in the `lode/` directory:

- [Overview](lode/overview.md) - Project architecture and goals
- [Event Storage](lode/event-storage.md) - Binary storage format specification
- [Events](lode/events.md) - Event types and binary formats

## License

MIT License - see [LICENSE](LICENSE) for details.
