/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:51:06 UTC 2020
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
      Comparator<Object> comparator0 = (Comparator<Object>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      Frequency frequency0 = new Frequency(comparator0);
      Frequency frequency1 = new Frequency();
      frequency0.getCumPct(0);
      frequency1.getPct(0);
      frequency1.getPct(0);
      frequency0.getPct(';');
      frequency0.getPct(0);
      frequency1.getSumFreq();
      Object object0 = new Object();
      frequency0.getCumPct(object0);
      // Undeclared exception!
      frequency0.addValue((Object) frequency1);
  }
}
