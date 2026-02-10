# FlixPatrol R Package

## API Reference

FlixPatrol API documentation: https://flixpatrol.com/api2/

### Query Parameter Operators

The API supports these filter operators:
- `[eq]` - exact match
- `[in]` - match any in list
- `[nin]` - not in list

The API does NOT support `[like]` for fuzzy matching.

### Authentication

All API requests use HTTP Basic Auth with the API key stored in the `FLIX_PATROL` environment variable.
