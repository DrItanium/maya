package clips

// #include "clips.h"
import "C"
import "unsafe"

type DataObject struct {
	ptr unsafe.Pointer
}
