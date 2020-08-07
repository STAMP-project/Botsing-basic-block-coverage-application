/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 17:45:58 UTC 2020
 */

package org.joda.time.field;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.joda.time.DateTimeFieldType;
import org.joda.time.Hours;
import org.joda.time.LocalDate;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class PreciseDurationDateTimeField_ESTest extends PreciseDurationDateTimeField_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      int int0 = 0;
      DateTimeFieldType.era();
      Hours hours0 = Hours.ONE;
      LocalDate localDate0 = LocalDate.now();
      localDate0.getMonthOfYear();
      // Undeclared exception!
      localDate0.withDayOfWeek(0);
  }
}
