
# Events in Convent

This document details the event types and their binary formats used in the Convent chat platform.

## Event Format Overview

Each event stored in an EventsPage begins with a 1-byte type identifier, followed by event-specific data. Events are stored contiguously without padding or alignment requirements.

## Currently Implemented Events

### Participant Joined (0x01)

This event records when a participant joins a chat. The event uses utility functions from ByteString module for consistent binary encoding/decoding. Its binary format is:

```
+---------------+--------------------+------------------+----------------------+
| Type (1 byte) | Timestamp (8 bytes)| ID (8 bytes)    | Name (up to 30 bytes)|
+---------------+--------------------+------------------+----------------------+
     0x01           Unix timestamp     Participant ID     UTF-8 encoded name
```

**Fields:**
- **Type**: Fixed value of 0x01
- **Timestamp**: 64-bit Unix timestamp in seconds
- **ID**: 64-bit unsigned integer identifying the participant
- **Name**: UTF-8 encoded text, maximum 30 bytes
  - Control characters are stripped
  - Names exceeding 30 bytes are truncated

### Participant Left (0x02)

This event records when a participant leaves a chat. Its binary format is:

```
+---------------+--------------------+------------------+
| Type (1 byte) | Timestamp (8 bytes)| ID (8 bytes)    |
+---------------+--------------------+------------------+
     0x02           Unix timestamp     Participant ID
```

**Fields:**
- **Type**: Fixed value of 0x02
- **Timestamp**: 64-bit Unix timestamp in seconds
- **ID**: 64-bit unsigned integer identifying the participant who left

### Message Submitted (0x03)

This event records when a participant sends a message. Its binary format is:

```
+---------------+--------------------+------------------+----------------------+
| Type (1 byte) | Timestamp (8 bytes)| ID (8 bytes)    | Message (≤1000 bytes)|
+---------------+--------------------+------------------+----------------------+
     0x03           Unix timestamp     Participant ID     UTF-8 encoded text
```

**Fields:**
- **Type**: Fixed value of 0x03
- **Timestamp**: 64-bit Unix timestamp in seconds
- **ID**: 64-bit unsigned integer identifying the message sender
- **Message**: UTF-8 encoded text, maximum 1000 bytes
  - Control characters are stripped
  - Messages exceeding 1000 bytes are truncated

## Event Storage

Events are stored in EventsPages (8192 bytes each) as detailed in [event-storage.md](event-storage.md). The page structure enables:
- Efficient random access to events
- Space reuse through compaction
- Sequential event scanning
