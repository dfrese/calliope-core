(ns dfrese.calliope.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            dfrese.calliope.core-test
            dfrese.calliope.impl.channel-test))

(doo-tests 'dfrese.calliope.core-test
           'dfrese.calliope.impl.channel-test)

