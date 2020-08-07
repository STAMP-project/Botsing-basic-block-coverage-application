/*
 * This file was automatically generated by EvoSuite
 * Sun May 17 18:39:43 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseCollection;
import com.xpn.xwiki.objects.classes.BaseClass;
import com.xpn.xwiki.objects.meta.LevelsMetaClass;
import java.util.HashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.internal.reference.ExplicitReferenceDocumentReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      baseClass0.addLevelsField("</>", "</>", 3103);
      baseClass0.getNumber();
      String string0 = "";
      baseClass0.addTextAreaField("</>", "", 62, 3103);
      HashMap<String, ExplicitReferenceDocumentReferenceResolver> hashMap0 = new HashMap<String, ExplicitReferenceDocumentReferenceResolver>();
      BiConsumer<Object, Object> biConsumer0 = (BiConsumer<Object, Object>) mock(BiConsumer.class, new ViolatedAssumptionAnswer());
      ExplicitReferenceDocumentReferenceResolver explicitReferenceDocumentReferenceResolver0 = new ExplicitReferenceDocumentReferenceResolver();
      EntityReferenceResolver<LevelsMetaClass> entityReferenceResolver0 = (EntityReferenceResolver<LevelsMetaClass>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(explicitReferenceDocumentReferenceResolver0, (Class<?>) ExplicitReferenceDocumentReferenceResolver.class, "entityReferenceResolver", (Object) entityReferenceResolver0);
      Injector.validateBean(explicitReferenceDocumentReferenceResolver0, (Class<?>) ExplicitReferenceDocumentReferenceResolver.class);
      hashMap0.put("</>", explicitReferenceDocumentReferenceResolver0);
      hashMap0.forEach(biConsumer0);
      // Undeclared exception!
      baseClass0.fromMap((Map<String, ?>) hashMap0, (BaseCollection) baseClass0);
  }
}
