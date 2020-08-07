/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 16:52:28 UTC 2020
 */

package org.apache.commons.math.stat;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Comparator;
import org.apache.commons.math.stat.Frequency;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class Frequency_ESTest extends Frequency_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      Comparator<String> comparator0 = null;
      Frequency frequency0 = new Frequency((Comparator) null);
      frequency0.valuesIterator();
      frequency0.getCount(0);
      frequency0.getSumFreq();
      frequency0.getCumPct((long) 0);
      frequency0.getCumFreq(210L);
      Object object0 = new Object();
      // Undeclared exception!
      frequency0.addValue(object0);
  }
}
