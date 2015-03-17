package clips

// #include "clips.h"
import "C"
import "unsafe"

type Fact struct {
	ptr unsafe.Pointer
}
