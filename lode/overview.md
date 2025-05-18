
# Lode - Event-Based Chat Platform

## Overview
Lode is a messaging platform built around an event-based storage system. It implements a efficient binary storage format for chat events with indexing capabilities for fast message retrieval.

## Architecture

### Currently Implemented Components

#### Event Storage
- **EventsPage**: Manages fixed-size (8192 bytes) pages containing chat events
  - Implements efficient binary storage of events
  - Handles event addition and retrieval
  - Maintains internal offset pointers for event locations
  - Provides space management functionality

- **IndexPage**: Manages index pages that map event offsets to their storage locations
  - Maintains segment information for quick event lookup
  - Each segment contains page offset and minimum event offset
  - Validates index integrity (ascending offsets, proper formatting)

- **EventStore**: Handles low-level file I/O operations
  - Provides functions to read and write pages to disk
  - Manages page-level operations

### Planned Components

#### Event Types
- Initial support for message events
- Extensible system for adding new event types
- Binary format with type ID prefix for parsing

#### Storage Structure
- Standardized directory structure per chat
- Each chat will have:
  - Events file containing message data
  - Index file for efficient lookup
- Fixed naming convention for files

#### HTTP API
- RESTful interface for chat interactions
- Operations for:
  - Message posting
  - Message retrieval
  - Chat management

## Important Terms

- **Event**: A piece of chat data (e.g., message, reaction, edit) with an offset
- **Page**: Fixed-size block (8192 bytes) containing events or index data
- **Offset**: Sequential identifier for events in a chat
- **Segment**: Index entry mapping event offsets to page locations
- **Event Type ID**: Binary prefix identifying how to parse an event

## Implementation Status

### Completed
- ✓ Basic page structure and management
- ✓ Event storage format
- ✓ Index page structure
- ✓ File I/O operations
- ✓ Page validation
- ✓ Space management

### To Be Implemented
- □ Event type system
- □ HTTP API
- □ Chat management
- □ Directory structure
- □ Additional event types
- □ Client interface
