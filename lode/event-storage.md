
# Event Storage Specification

This document details the low-level storage format used by Convent for storing chat events.

## Overview

Chat events are stored in a binary file using fixed-size pages of 8192 bytes each. This design enables:
- Efficient random access to events
- Easy page allocation and deallocation
- Simple memory mapping capabilities
- Consistent I/O patterns

## Page Structure

### Events Page Layout
```
+----------------+---------------------+------------------------+
| Reserve (2B)   | Event Pointers     |    Reserve Space      | Event Data |
+----------------+---------------------+------------------------|------------|
```

Each page consists of:

1. **Reserve Pointer** (2 bytes)
   - Marks the boundary between event pointers and unused space
   - Always even-numbered (aligned to 2 bytes)
   - Minimum value: 2 (empty page)
   - Maximum value: 8192
   - May equal the last event pointer when page is completely full

2. **Event Pointers** (2 bytes each)
   - Array of pointers to event data
   - Starts at byte 2
   - Grows forward from start of page
   - Each pointer is a 16-bit unsigned integer
   - Points to the start of corresponding event data
   - Stored in ascending order

3. **Reserve Space**
   - Empty space between event pointers and event data
   - Size = Last event pointer - Reserve pointer
   - Zero bytes when page is completely full

4. **Event Data**
   - Actual event content
   - Grows backward from end of page
   - Each event is stored contiguously
   - No padding or alignment requirements
   - First event added to page ends at page boundary (8192)

### Space Management

- Free space = End pointer of last event - Reserve pointer
- New events are added only if there's enough space for:
  - The event pointer (2 bytes)
  - The event data itself

## Event Storage Rules

1. **Event Pointer Rules**
   - Must be greater than or equal to the reserve pointer
   - Must be strictly less than 8192 (page boundary)
   - Must be in descending order (each pointer greater than the next)
   - Events are stored contiguously (no gaps between events)

2. **Event Data Rules**
   - Stored in reverse order from end of page
   - No size prefix or metadata
   - Size determined by pointer arithmetic
   - Last event ends at page boundary (8192)

## Example Page Layout

For a page containing three events:
```
Position  Content                 Description
0000      0008                   Reserve pointer (8)
0002      8180                   Pointer to Event 3 (added last)
0004      8190                   Pointer to Event 2
0006      8192                   Pointer to Event 1 (added first)
0008      ...                    Reserve space
8180      [Event 3 data]         10 bytes of event data
8190      [Event 2 data]         2 bytes of event data
8192      [End of Event 1]       Event 1 ends at page boundary
```

## Page Validation

When reading a page, the following is validated:

1. Page size must be exactly 8192 bytes
2. Reserve pointer must be:
   - ≥ 2 bytes
   - ≤ 8192 bytes
   - Even-numbered
3. Event pointers must be:
   - In ascending order
   - Greater than reserve pointer
   - Less than or equal to 8192
4. No gaps allowed between events

## Error Conditions

The following conditions result in page read errors:
- Invalid page size
- Invalid reserve pointer value
- Invalid event pointer values
- Non-ascending event pointers
- Pointers into reserved space

## Implementation Notes

- All integers are stored in big-endian format
- No compression is used
- Events are stored in raw format
- Page modifications are atomic
- Zero-copy reading is possible via memory mapping

This specification is implemented in `Web.Convent.Storage.EventsPage` and related modules.

# Index Structure

## Index Page Layout

Index pages are also 8192 bytes and contain fixed-size segments that map event offsets to their corresponding page locations. This enables efficient lookup of events by their offset.

### Index Page Format
```
+----------------+----------------+----------------+---
| Entry 1        | Entry 2        | Entry 3        | ...
+----------------+----------------+----------------+---
```

Each index page contains:

1. **Fixed-size Entries** (16 bytes each)
   - Maximum 512 entries per page (8192/16 bytes)
   - Each entry contains:
     - Page offset (8 bytes): Location of events page in store
     - Minimum event offset (8 bytes): Smallest event offset in that page

2. **Zero Padding**
   - Unused entries are zeroed
   - First all-zero entry marks end of valid data
   - All subsequent entries must also be zero

### Index Page Rules

1. **Entry Ordering**
   - First entry must refer to the second events page (offset=1)
   - Page offsets must be strictly ascending
   - Event offsets must be strictly ascending
   - No gaps allowed between valid entries
   - All entries after first zero entry must be zero

2. **Validation**
   - Page size must be exactly 8192 bytes
   - Maximum 64 segments per page
   - Non-zero segments cannot appear after zero segments
   - Both page and event offsets in segment must be zero or both non-zero

### Example Index Page Layout

For an index page with three segments:
```
Position  Content                 Description
0000      0000000000000010      Page offset for first segment
0008      0000000000000100      Minimum event offset in first page
0016      0000000000000020      Page offset for second segment
0024      0000000000000200      Minimum event offset in second page
0032      0000000000000030      Page offset for third segment
0040      0000000000000300      Minimum event offset in third page
0048      0000000000000000      Start of zero padding (unused segments)
....      0000000000000000      (continues to end of page)
```

This specification is implemented in `Web.Convent.Storage.EventsPage` and related modules.
