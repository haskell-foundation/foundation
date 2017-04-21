# Advanced Usage Options

## Environment Variables

You can override some of the constants used by *Foundation* using environment
variables:

### Maximum size of unpinned arrays (in bytes)

- Environment Variable: `HS_FOUNDATION_UARRAY_UNPINNED_MAX`
- Default: `1024`

When memory for a new array is allocated, we decide if that memory region should
be **pinned** (will not be copied around by GC) or **unpinned** (can be moved
around by GC) depending on the size of the memory region. This value specifies
up to which size **unpinned** memory will be used.
