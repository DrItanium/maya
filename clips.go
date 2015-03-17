package clips

// #cgo LDFLAGS: -lm -lc
// #include "clips.h"
import "C"
import "unsafe"

type StrategyKind int

const (
	DepthStrategy = iota
	BreadthStrategy
	LexStrategy
	MeaStrategy
	ComplexityStrategy
	SimplicityStrategy
	RandomStrategy
)

type Environment struct {
	ptr unsafe.Pointer
}

func (this *Environment) DestroyEnvironment() {
	C.DestroyEnvironment(this.ptr)
	this.ptr = nil
}

func (this *Environment) Reset() {
	C.EnvReset(this.ptr)
}

func (this *Environment) Run(runLimit int64) int64 {
	return int64(C.EnvRun(this.ptr, C.longlong(runLimit)))
}

func (this *Environment) Clear() {
	C.EnvClear(this.ptr)
}

func (this *Environment) Build(constructString string) bool {
	str := C.CString(constructString)
	defer C.free((unsafe.Pointer)(str))
	return C.EnvBuild(this.ptr, str) == 1
}

func (this *Environment) SetStrategy(strategy StrategyKind) StrategyKind {
	return StrategyKind(C.EnvSetStrategy(this.ptr, C.int(strategy)))
}

func (this *Environment) GetStrategy() StrategyKind {
	return StrategyKind(C.EnvGetStrategy(this.ptr))
}
