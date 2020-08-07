/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 21:30:37 UTC 2020
 */

package org.apache.commons.lang3;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.function.Consumer;
import org.apache.commons.lang3.StringUtils;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class StringUtils_ESTest extends StringUtils_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      LinkedList<Integer> linkedList0 = new LinkedList<Integer>();
      Iterator<Integer> iterator0 = linkedList0.descendingIterator();
      linkedList0.add((Integer) null);
      StringUtils.join(iterator0, 'r');
      linkedList0.add((Integer) null);
      linkedList0.descendingIterator();
      ArrayDeque<String> arrayDeque0 = new ArrayDeque<String>();
      Consumer<Object> consumer0 = (Consumer<Object>) mock(Consumer.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(consumer0).toString();
      String string0 = " ";
      Object[] objectArray0 = new Object[9];
      objectArray0[0] = (Object) consumer0;
      objectArray0[1] = (Object) arrayDeque0;
      objectArray0[2] = (Object) iterator0;
      objectArray0[3] = (Object) consumer0;
      objectArray0[4] = (Object) iterator0;
      objectArray0[5] = (Object) null;
      objectArray0[6] = (Object) null;
      objectArray0[7] = (Object) consumer0;
      objectArray0[8] = (Object) "";
      String string1 = "iEn{*m`H6Z<J5k\"7";
      // Undeclared exception!
      StringUtils.join(objectArray0, "iEn{*m`H6Z<J5k\"7");
  }
}
