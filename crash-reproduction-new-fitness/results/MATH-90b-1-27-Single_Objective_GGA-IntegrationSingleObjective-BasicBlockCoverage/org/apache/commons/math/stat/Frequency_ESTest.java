/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 03:29:10 UTC 2020
 */

package org.apache.commons.math.stat;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.stat.Frequency;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Frequency_ESTest extends Frequency_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Comparator<String> comparator0 = (Comparator<String>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      doReturn(0).when(comparator0).compare(anyString() , anyString());
      Frequency frequency0 = new Frequency(comparator0);
      frequency0.getCount((-2629L));
      Frequency frequency1 = new Frequency();
      Object object0 = new Object();
      frequency1.getCumFreq(object0);
      frequency1.getCumFreq(1619L);
      Integer integer0 = new Integer(0);
      frequency0.addValue((Object) integer0);
      Comparator<Long> comparator1 = (Comparator<Long>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      Frequency frequency2 = new Frequency(comparator1);
      frequency1.getCumPct(1546L);
      // Undeclared exception!
      frequency1.addValue((Object) frequency0);
  }
}
