(ns dfrese.calliope.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            dfrese.calliope.core-test
            dfrese.calliope.channel-test
            dfrese.calliope.app-test))

(doo-tests 'dfrese.calliope.core-test
           'dfrese.calliope.channel-test
           'dfrese.calliope.app-test)

