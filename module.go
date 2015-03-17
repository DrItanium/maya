package clips

// #include "clips.h"
import "C"
import "unsafe"

type Module struct {
	ptr unsafe.Pointer
}
