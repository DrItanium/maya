package clips

// #cgo LDFLAGS: -lm -lc
// #include "clips.h"
import "C"
import "unsafe"
import "fmt"

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

func (this *Environment) DribbleOn(fileName string) bool {
	str := C.CString(fileName)
	defer C.free((unsafe.Pointer)(str))
	return C.EnvDribbleOn(this.ptr, str) == 1
}

func (this *Environment) DribbleOff() bool {
	return C.EnvDribbleOff(this.ptr) == 1
}

func (this *Environment) DribbleActive() bool {
	return C.EnvDribbleActive(this.ptr) == 1
}

const (
	WatchFacts            = "facts"
	WatchRules            = "rules"
	WatchActivations      = "activations"
	WatchFocus            = "focus"
	WatchCompilations     = "compilations"
	WatchStatistics       = "statistics"
	WatchGlobals          = "globals"
	WatchInstances        = "instances"
	WatchSlots            = "slots"
	WatchMessages         = "messages"
	WatchMessageHandlers  = "message-handlers"
	WatchGenericFunctions = "generic-functions"
	WatchMethods          = "methods"
	WatchDeffunctions     = "deffunctions"
	WatchAll              = "all"
)

var watchItems = []string{
	WatchFacts,
	WatchRules,
	WatchActivations,
	WatchFocus,
	WatchCompilations,
	WatchStatistics,
	WatchGlobals,
	WatchInstances,
	WatchSlots,
	WatchMessages,
	WatchMessageHandlers,
	WatchGenericFunctions,
	WatchMethods,
	WatchDeffunctions,
	WatchAll,
}

func (this *Environment) GetWatchItem(item string) (bool, error) {
	str := C.CString(item)
	defer C.free((unsafe.Pointer)(str))
	result := int(C.EnvGetWatchItem(this.ptr, str))
	if result == -1 {
		return false, fmt.Errorf("Watch item %s does not exist", item)
	} else {
		return (result == 1), nil
	}
}
func legalWatchItem(item string) bool {
	for _, value := range watchItems {
		if value == item {
			return true
		}
	}
	return false
}
func (this *Environment) Watch(item string) (bool, error) {
	if !legalWatchItem(item) {
		return false, fmt.Errorf("Illegal watch item %s", item)
	}
	str := C.CString(item)
	defer C.free((unsafe.Pointer)(str))
	return int(C.EnvWatch(this.ptr, str)) == 1, nil
}

func (this *Environment) Unwatch(item string) (bool, error) {
	if !legalWatchItem(item) {
		return false, fmt.Errorf("Illegal watch item %s", item)
	}
	str := C.CString(item)
	defer C.free((unsafe.Pointer)(str))
	return int(C.EnvUnwatch(this.ptr, str)) == 1, nil
}
