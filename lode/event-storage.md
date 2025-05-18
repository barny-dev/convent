
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
