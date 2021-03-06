(unwatch all)
(clear)
(defglobal ?*sorstate* = (set-sequence-operator-recognition TRUE))
(dribble-on "Actual//dfnxerr.out")
(batch "dfnxerr.bat")
(dribble-off)
(set-sequence-operator-recognition ?*sorstate*))
(clear)
(open "Results//dfnxerr.rsl" dfnxerr "w")
(load "compline.clp")
(printout dfnxerr "dfnxerr.bat differences are as follows:" crlf)
(compare-files "Expected//dfnxerr.out" "Actual//dfnxerr.out" dfnxerr)
(close dfnxerr)
