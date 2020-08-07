/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:11:11 UTC 2020
 */

package org.xwiki.configuration.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import java.awt.List;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.jfree.data.xy.XYDatasetTableModel;
import org.junit.runner.RunWith;
import org.slf4j.helpers.NOPLogger;
import org.xwiki.bridge.DocumentAccessBridge;
import org.xwiki.cache.CacheManager;
import org.xwiki.configuration.internal.AbstractDocumentConfigurationSource;
import org.xwiki.configuration.internal.SpacePreferencesConfigurationSource;
import org.xwiki.model.reference.EntityReferenceSerializer;
import org.xwiki.observation.ObservationManager;
import org.xwiki.properties.ConverterManager;
import org.xwiki.wiki.descriptor.WikiDescriptorManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractDocumentConfigurationSource_ESTest extends AbstractDocumentConfigurationSource_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SpacePreferencesConfigurationSource spacePreferencesConfigurationSource0 = new SpacePreferencesConfigurationSource();
      DocumentAccessBridge documentAccessBridge0 = mock(DocumentAccessBridge.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) SpacePreferencesConfigurationSource.class, "documentAccessBridge", (Object) documentAccessBridge0);
      CacheManager cacheManager0 = mock(CacheManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "cacheManager", (Object) cacheManager0);
      ConverterManager converterManager0 = mock(ConverterManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "converter", (Object) converterManager0);
      NOPLogger nOPLogger0 = NOPLogger.NOP_LOGGER;
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "logger", (Object) nOPLogger0);
      ObservationManager observationManager0 = mock(ObservationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "observation", (Object) observationManager0);
      EntityReferenceSerializer<List> entityReferenceSerializer0 = (EntityReferenceSerializer<List>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "referenceSerializer", (Object) entityReferenceSerializer0);
      WikiDescriptorManager wikiDescriptorManager0 = mock(WikiDescriptorManager.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(wikiDescriptorManager0).getCurrentWikiId();
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "wikiManager", (Object) wikiDescriptorManager0);
      Provider<Integer> provider0 = (Provider<Integer>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "xcontextProvider", (Object) provider0);
      Injector.validateBean(spacePreferencesConfigurationSource0, (Class<?>) SpacePreferencesConfigurationSource.class);
      XYDatasetTableModel xYDatasetTableModel0 = new XYDatasetTableModel();
      spacePreferencesConfigurationSource0.isEmpty((Object) xYDatasetTableModel0);
      // Undeclared exception!
      spacePreferencesConfigurationSource0.getCurrentWikiReference();
  }
}
