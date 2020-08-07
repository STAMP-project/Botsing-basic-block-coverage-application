/*
 * This file was automatically generated by EvoSuite
 * Mon May 18 05:22:29 UTC 2020
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.util.ArrayDeque;
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
import org.xwiki.extension.version.internal.DefaultVersion;
import org.xwiki.extension.version.internal.DefaultVersionConstraint;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest extends AbstractInstallPlanJob$ModifableExtensionPlanTree_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree1 = abstractInstallPlanJob_ModifableExtensionPlanTree0.clone();
      DefaultVersion defaultVersion0 = new DefaultVersion("");
      DefaultVersion defaultVersion1 = new DefaultVersion(defaultVersion0);
      ExtensionId extensionId0 = new ExtensionId("", defaultVersion1);
      EmptyExtension emptyExtension0 = new EmptyExtension(extensionId0, "");
      ArrayDeque<InstalledExtension> arrayDeque0 = new ArrayDeque<InstalledExtension>();
      ExtensionPlanAction.Action extensionPlanAction_Action0 = ExtensionPlanAction.Action.UNINSTALL;
      DefaultExtensionPlanAction defaultExtensionPlanAction0 = new DefaultExtensionPlanAction(emptyExtension0, arrayDeque0, extensionPlanAction_Action0, (String) null, true);
      DefaultVersionConstraint defaultVersionConstraint0 = new DefaultVersionConstraint(defaultVersion1);
      DefaultVersionConstraint defaultVersionConstraint1 = new DefaultVersionConstraint(defaultVersionConstraint0);
      defaultExtensionPlanAction0.toString();
      DefaultExtensionPlanNode defaultExtensionPlanNode0 = new DefaultExtensionPlanNode(defaultExtensionPlanAction0, defaultVersionConstraint1);
      abstractInstallPlanJob_ModifableExtensionPlanTree0.addIfAbsent(defaultExtensionPlanNode0);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.toArray();
      abstractInstallPlanJob_ModifableExtensionPlanTree1.removeAll(abstractInstallPlanJob_ModifableExtensionPlanTree1);
      abstractInstallPlanJob_ModifableExtensionPlanTree1.addAllAbsent(abstractInstallPlanJob_ModifableExtensionPlanTree1);
      // Undeclared exception!
      abstractInstallPlanJob_ModifableExtensionPlanTree0.clone();
  }
}
