/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 07:19:46 UTC 2020
 */

package org.xwiki.rendering.listener.chaining;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.Map;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.junit.runner.RunWith;
import org.xwiki.rendering.listener.Format;
import org.xwiki.rendering.listener.Listener;
import org.xwiki.rendering.listener.MetaData;
import org.xwiki.rendering.listener.chaining.BlockStateChainingListener;
import org.xwiki.rendering.listener.chaining.ChainingListener;
import org.xwiki.rendering.listener.chaining.EmptyBlockChainingListener;
import org.xwiki.rendering.listener.chaining.ListenerChain;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractChainingListener_ESTest extends AbstractChainingListener_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ListenerChain listenerChain0 = new ListenerChain();
      BlockStateChainingListener blockStateChainingListener0 = new BlockStateChainingListener(listenerChain0);
      EmptyBlockChainingListener emptyBlockChainingListener0 = new EmptyBlockChainingListener(listenerChain0);
      ListenerChain listenerChain1 = mock(ListenerChain.class, new ViolatedAssumptionAnswer());
      doReturn(" %vni58oz&K").when(listenerChain1).toString();
      doReturn(blockStateChainingListener0).when(listenerChain1).getNextListener(nullable(java.lang.Class.class));
      BlockStateChainingListener blockStateChainingListener1 = new BlockStateChainingListener(listenerChain1);
      listenerChain0.addListener(blockStateChainingListener1);
      MetaData metaData0 = MetaData.EMPTY;
      metaData0.getMetaData();
      metaData0.addMetaData(" %vni58oz&K", (Object) " %vni58oz&K");
      Map<String, String> map0 = Listener.EMPTY_PARAMETERS;
      BlockStateChainingListener blockStateChainingListener2 = new BlockStateChainingListener(listenerChain0);
      Class<ChainingListener> class0 = ChainingListener.class;
      listenerChain0.popListener(class0);
      EmptyBlockChainingListener emptyBlockChainingListener1 = new EmptyBlockChainingListener(listenerChain0);
      MetaData metaData1 = new MetaData();
      BlockStateChainingListener blockStateChainingListener3 = new BlockStateChainingListener(listenerChain0);
      listenerChain0.addListener(emptyBlockChainingListener0);
      MetaData metaData2 = MetaData.EMPTY;
      Format format0 = Format.NONE;
      Map<String, String> map1 = Listener.EMPTY_PARAMETERS;
      Format format1 = Format.SUPERSCRIPT;
      // Undeclared exception!
      blockStateChainingListener1.endFormat(format1, map1);
  }
}
