
# Events in Convent

This document details the event types and their binary formats used in the Convent chat platform.

## Event Format Overview

Each event stored in an EventsPage begins with a 1-byte type identifier, followed by event-specific data. Events are stored contiguously without padding or alignment requirements.

## Currently Implemented Events

### Participant Joined (0x01)

This event records when a participant joins a chat. The event uses utility functions from ByteString module for consistent binary encoding/decoding. Its binary format is:

```
+---------------+------------------+----------------------+
| Type (1 byte) | ID (8 bytes)    | Name (up to 30 bytes)|
+---------------+------------------+----------------------+
     0x01         Participant ID     UTF-8 encoded name
```

**Fields:**
- **Type**: Fixed value of 0x01
- **ID**: 64-bit unsigned integer identifying the participant
- **Name**: UTF-8 encoded text, maximum 30 bytes
  - Control characters are stripped
  - Names exceeding 30 bytes are truncated

## Event Storage

Events are stored in EventsPages (8192 bytes each) as detailed in [event-storage.md](event-storage.md). The page structure enables:
- Efficient random access to events
- Space reuse through compaction
- Sequential event scanning
