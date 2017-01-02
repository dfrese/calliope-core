(ns calliope.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            calliope.core-test
            calliope.channel-test
            calliope.app-test
            calliope.browser-test))

(doo-tests 'calliope.core-test
           'calliope.channel-test
           'calliope.app-test
           'calliope.browser-test)

