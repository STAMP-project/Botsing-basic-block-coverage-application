/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 05:18:49 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.ListIterator;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.extension.job.internal.AbstractInstallPlanJob;
import org.xwiki.extension.job.plan.ExtensionPlanNode;
import org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest extends AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree1 = abstractInstallPlanJob_ModifableExtensionPlanTree0.clone();
      ListIterator<ExtensionPlanNode> listIterator0 = abstractInstallPlanJob_ModifableExtensionPlanTree0.listIterator();
      abstractInstallPlanJob_ModifableExtensionPlanTree0.contains(abstractInstallPlanJob_ModifableExtensionPlanTree1);
      DefaultExtensionPlanNode defaultExtensionPlanNode0 = new DefaultExtensionPlanNode();
      abstractInstallPlanJob_ModifableExtensionPlanTree0.addIfAbsent(defaultExtensionPlanNode0);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.addAllAbsent(abstractInstallPlanJob_ModifableExtensionPlanTree0);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.removeAll(abstractInstallPlanJob_ModifableExtensionPlanTree0);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.contains(defaultExtensionPlanNode0);
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree2 = abstractInstallPlanJob_ModifableExtensionPlanTree1.clone();
      abstractInstallPlanJob_ModifableExtensionPlanTree1.listIterator();
      abstractInstallPlanJob_ModifableExtensionPlanTree1.contains(listIterator0);
      DefaultExtensionPlanNode defaultExtensionPlanNode1 = new DefaultExtensionPlanNode();
      abstractInstallPlanJob_ModifableExtensionPlanTree1.addIfAbsent(defaultExtensionPlanNode1);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.addAllAbsent(abstractInstallPlanJob_ModifableExtensionPlanTree2);
      abstractInstallPlanJob_ModifableExtensionPlanTree0.isEmpty();
      abstractInstallPlanJob_ModifableExtensionPlanTree1.contains(listIterator0);
      abstractInstallPlanJob_ModifableExtensionPlanTree2.toString();
      // Undeclared exception!
      abstractInstallPlanJob_ModifableExtensionPlanTree1.clone();
  }
}
