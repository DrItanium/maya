#include <cstdint>

template<typename T> T combine(T a, T b) { return a + b; }

#define X(typ) template typ combine (typ, typ)

X(uint8_t);
X(int8_t);
X(uint16_t);
X(int16_t);

#undef X
