#include "Rts.h"

#if __GLASGOW_HASKELL__ < 802
int foundation_is_bytearray_pinned(void *p)
{
    return Bdescr((StgPtr) p)->flags & BF_PINNED;
}
#endif
