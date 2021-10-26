/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 22:36:32 UTC 2021
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.PriorityQueue;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.junit.runner.RunWith;
import org.xwiki.extension.ExtensionId;
import org.xwiki.extension.InstalledExtension;
import org.xwiki.extension.job.internal.AbstractInstallPlanJob;
import org.xwiki.extension.job.plan.ExtensionPlanAction;
import org.xwiki.extension.job.plan.internal.DefaultExtensionPlanAction;
import org.xwiki.extension.job.plan.internal.DefaultExtensionPlanNode;
import org.xwiki.extension.test.EmptyExtension;
import org.xwiki.extension.version.VersionRangeCollection;
import org.xwiki.extension.version.internal.DefaultVersion;
import org.xwiki.extension.version.internal.DefaultVersionConstraint;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest extends AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      ExtensionId extensionId0 = new ExtensionId("", "");
      EmptyExtension emptyExtension0 = new EmptyExtension(extensionId0, "");
      PriorityQueue<InstalledExtension> priorityQueue0 = new PriorityQueue<InstalledExtension>();
      ExtensionPlanAction.Action extensionPlanAction_Action0 = ExtensionPlanAction.Action.NONE;
      emptyExtension0.setWebsite("");
      DefaultExtensionPlanAction defaultExtensionPlanAction0 = new DefaultExtensionPlanAction(emptyExtension0, priorityQueue0, extensionPlanAction_Action0, "Unable to add cloned data item.", false);
      PriorityQueue<VersionRangeCollection> priorityQueue1 = new PriorityQueue<VersionRangeCollection>();
      DefaultVersion defaultVersion0 = new DefaultVersion("vOB4}D 1|)LLc");
      DefaultVersionConstraint defaultVersionConstraint0 = new DefaultVersionConstraint(priorityQueue1, defaultVersion0);
      DefaultExtensionPlanNode defaultExtensionPlanNode0 = new DefaultExtensionPlanNode(defaultExtensionPlanAction0, abstractInstallPlanJob_ModifableExtensionPlanTree0, defaultVersionConstraint0);
      abstractInstallPlanJob_ModifableExtensionPlanTree0.addIfAbsent(defaultExtensionPlanNode0);
      // Undeclared exception!
      abstractInstallPlanJob_ModifableExtensionPlanTree0.clone();
  }
}
