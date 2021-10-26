/*
 * This file was automatically generated by EvoSuite
 * Mon Oct 25 22:44:22 UTC 2021
 */

package org.xwiki.extension.job.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.component.manager.ComponentManager;
import org.xwiki.component.namespace.NamespaceValidator;
import org.xwiki.extension.ExtensionId;
import org.xwiki.extension.handler.ExtensionHandlerManager;
import org.xwiki.extension.job.internal.AbstractExtensionJob;
import org.xwiki.extension.job.internal.AbstractInstallPlanJob;
import org.xwiki.extension.job.internal.UpgradePlanJob;
import org.xwiki.extension.job.plan.ExtensionPlanAction;
import org.xwiki.extension.job.plan.ExtensionPlanNode;
import org.xwiki.extension.repository.CoreExtensionRepository;
import org.xwiki.extension.repository.ExtensionRepositoryManager;
import org.xwiki.extension.repository.InstalledExtensionRepository;
import org.xwiki.extension.repository.LocalExtensionRepository;
import org.xwiki.logging.marker.TranslationMarker;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class UpgradePlanJob_ESTest extends UpgradePlanJob_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ExtensionPlanAction extensionPlanAction0 = mock(ExtensionPlanAction.class, new ViolatedAssumptionAnswer());
      TranslationMarker translationMarker0 = new TranslationMarker("Failed to resolve remote versions for extension ad [{}]");
      UpgradePlanJob upgradePlanJob0 = new UpgradePlanJob();
      CoreExtensionRepository coreExtensionRepository0 = mock(CoreExtensionRepository.class, new ViolatedAssumptionAnswer());
      Injector.inject(upgradePlanJob0, (Class<?>) AbstractInstallPlanJob.class, "coreExtensionRepository", (Object) coreExtensionRepository0);
      NamespaceValidator namespaceValidator0 = mock(NamespaceValidator.class, new ViolatedAssumptionAnswer());
      Injector.inject(upgradePlanJob0, (Class<?>) AbstractInstallPlanJob.class, "namespaceResolver", (Object) namespaceValidator0);
      ExtensionRepositoryManager extensionRepositoryManager0 = mock(ExtensionRepositoryManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(upgradePlanJob0, (Class<?>) AbstractInstallPlanJob.class, "repositoryManager", (Object) extensionRepositoryManager0);
      ExtensionHandlerManager extensionHandlerManager0 = mock(ExtensionHandlerManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(upgradePlanJob0, (Class<?>) AbstractExtensionJob.class, "extensionHandlerManager", (Object) extensionHandlerManager0);
      InstalledExtensionRepository installedExtensionRepository0 = mock(InstalledExtensionRepository.class, new ViolatedAssumptionAnswer());
      Injector.inject(upgradePlanJob0, (Class<?>) AbstractExtensionJob.class, "installedExtensionRepository", (Object) installedExtensionRepository0);
      LocalExtensionRepository localExtensionRepository0 = mock(LocalExtensionRepository.class, new ViolatedAssumptionAnswer());
      Injector.inject(upgradePlanJob0, (Class<?>) AbstractExtensionJob.class, "localExtensionRepository", (Object) localExtensionRepository0);
      AbstractInstallPlanJob.ModifableExtensionPlanTree abstractInstallPlanJob_ModifableExtensionPlanTree0 = new AbstractInstallPlanJob.ModifableExtensionPlanTree();
      upgradePlanJob0.extensionTree = abstractInstallPlanJob_ModifableExtensionPlanTree0;
      ComponentManager componentManager0 = mock(ComponentManager.class, new ViolatedAssumptionAnswer());
      ExtensionPlanNode extensionPlanNode0 = mock(ExtensionPlanNode.class, new ViolatedAssumptionAnswer());
      abstractInstallPlanJob_ModifableExtensionPlanTree0.add(extensionPlanNode0);
      // Undeclared exception!
      upgradePlanJob0.tryInstallExtension((ExtensionId) null, "09Z`k bQPB`ZW/xo");
  }
}
