/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:52:45 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseCollection;
import com.xpn.xwiki.objects.classes.BaseClass;
import java.util.Map;
import java.util.concurrent.ConcurrentSkipListMap;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.internal.reference.DefaultStringDocumentReferenceResolver;
import org.xwiki.model.internal.reference.ExplicitReferenceDocumentReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      BaseClass baseClass1 = new BaseClass();
      ConcurrentSkipListMap<String, DefaultStringDocumentReferenceResolver> concurrentSkipListMap0 = new ConcurrentSkipListMap<String, DefaultStringDocumentReferenceResolver>();
      DefaultStringDocumentReferenceResolver defaultStringDocumentReferenceResolver0 = new DefaultStringDocumentReferenceResolver();
      EntityReferenceResolver<ExplicitReferenceDocumentReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<ExplicitReferenceDocumentReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultStringDocumentReferenceResolver0, (Class<?>) DefaultStringDocumentReferenceResolver.class, "entityReferenceResolver", (Object) entityReferenceResolver0);
      Injector.validateBean(defaultStringDocumentReferenceResolver0, (Class<?>) DefaultStringDocumentReferenceResolver.class);
      concurrentSkipListMap0.put(".ok0n '*=A`gQuIJ45", defaultStringDocumentReferenceResolver0);
      baseClass1.addDBTreeListField(".ok0n '*=A`gQuIJ45", ".ok0n '*=A`gQuIJ45", "displayFormType");
      ConcurrentSkipListMap<String, DefaultStringDocumentReferenceResolver> concurrentSkipListMap1 = concurrentSkipListMap0.clone();
      concurrentSkipListMap1.higherKey("dv5om#;zmfys@");
      // Undeclared exception!
      baseClass1.fromMap((Map<String, ?>) concurrentSkipListMap1, (BaseCollection) baseClass0);
  }
}
