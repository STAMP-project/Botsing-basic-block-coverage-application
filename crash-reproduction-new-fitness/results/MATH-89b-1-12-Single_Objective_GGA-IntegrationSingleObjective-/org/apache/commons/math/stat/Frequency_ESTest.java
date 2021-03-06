/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 12:27:58 UTC 2020
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
      Comparator<Integer> comparator0 = (Comparator<Integer>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      Frequency frequency0 = new Frequency(comparator0);
      char char0 = '9';
      frequency0.getCumPct('9');
      long long0 = (-1612L);
      frequency0.getCount((-1612L));
      frequency0.getCumFreq('[');
      frequency0.getCumPct('[');
      frequency0.getCount('9');
      frequency0.getCount(3412L);
      frequency0.getSumFreq();
      Object object0 = new Object();
      frequency0.getCumFreq(object0);
      // Undeclared exception!
      frequency0.addValue(object0);
  }
}
