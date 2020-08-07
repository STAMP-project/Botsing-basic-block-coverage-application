/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 17:58:34 UTC 2020
 */

package org.xwiki.configuration.internal;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
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
      CacheManager cacheManager0 = mock(CacheManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "cacheManager", (Object) cacheManager0);
      ConverterManager converterManager0 = mock(ConverterManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "converter", (Object) converterManager0);
      Logger logger0 = mock(Logger.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "logger", (Object) logger0);
      ObservationManager observationManager0 = mock(ObservationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "observation", (Object) observationManager0);
      EntityReferenceSerializer<Integer> entityReferenceSerializer0 = (EntityReferenceSerializer<Integer>) mock(EntityReferenceSerializer.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "referenceSerializer", (Object) entityReferenceSerializer0);
      WikiDescriptorManager wikiDescriptorManager0 = mock(WikiDescriptorManager.class, new ViolatedAssumptionAnswer());
      doReturn((String) null).when(wikiDescriptorManager0).getCurrentWikiId();
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "wikiManager", (Object) wikiDescriptorManager0);
      Provider<String> provider0 = (Provider<String>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class, "xcontextProvider", (Object) provider0);
      Injector.validateBean(spacePreferencesConfigurationSource0, (Class<?>) AbstractDocumentConfigurationSource.class);
      // Undeclared exception!
      spacePreferencesConfigurationSource0.getCurrentWikiReference();
  }
}
