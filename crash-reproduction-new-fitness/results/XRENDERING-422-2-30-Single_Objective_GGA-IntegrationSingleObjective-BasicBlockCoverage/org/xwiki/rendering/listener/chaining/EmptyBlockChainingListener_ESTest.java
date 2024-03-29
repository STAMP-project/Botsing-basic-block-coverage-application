/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 08:09:01 UTC 2020
 */

package org.xwiki.rendering.listener.chaining;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.HashMap;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.rendering.listener.Format;
import org.xwiki.rendering.listener.chaining.ChainingListener;
import org.xwiki.rendering.listener.chaining.EmptyBlockChainingListener;
import org.xwiki.rendering.listener.chaining.ListenerChain;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class EmptyBlockChainingListener_ESTest extends EmptyBlockChainingListener_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ListenerChain listenerChain0 = mock(ListenerChain.class, new ViolatedAssumptionAnswer());
      doReturn((ChainingListener) null).when(listenerChain0).getNextListener(nullable(java.lang.Class.class));
      EmptyBlockChainingListener emptyBlockChainingListener0 = new EmptyBlockChainingListener(listenerChain0);
      Format format0 = Format.STRIKEDOUT;
      HashMap<String, String> hashMap0 = new HashMap<String, String>();
      // Undeclared exception!
      emptyBlockChainingListener0.endFormat(format0, hashMap0);
  }
}
