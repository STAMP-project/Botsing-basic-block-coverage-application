/*
 * This file was automatically generated by EvoSuite
 * Fri Jan 17 23:25:09 UTC 2020
 */

package org.joda.time.chrono;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.MonthDay;
import org.joda.time.chrono.CopticChronology;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseChronology_ESTest extends BaseChronology_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      MonthDay monthDay0 = MonthDay.parse("");
      MonthDay monthDay1 = monthDay0.minusDays(1);
      CopticChronology copticChronology0 = CopticChronology.getInstanceUTC();
      // Undeclared exception!
      copticChronology0.set(monthDay1, (-86400000L));
  }
}
