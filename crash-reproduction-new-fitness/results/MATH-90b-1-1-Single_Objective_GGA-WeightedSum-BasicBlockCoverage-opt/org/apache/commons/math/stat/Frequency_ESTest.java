/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 21:31:24 UTC 2021
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
      Comparator<Long> comparator0 = (Comparator<Long>) mock(Comparator.class, new ViolatedAssumptionAnswer());
      Frequency frequency0 = new Frequency(comparator0);
      frequency0.getPct('L');
      Integer integer0 = new Integer(3119);
      frequency0.getPct((Object) integer0);
      Frequency frequency1 = new Frequency();
      frequency1.addValue(934);
      frequency0.valuesIterator();
      Object object0 = new Object();
      // Undeclared exception!
      frequency1.addValue(object0);
  }
}
