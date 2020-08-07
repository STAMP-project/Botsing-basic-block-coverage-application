/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 14:16:53 UTC 2020
 */

package com.xpn.xwiki.objects.classes;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.objects.BaseCollection;
import com.xpn.xwiki.objects.classes.BaseClass;
import com.xpn.xwiki.objects.meta.EmailMetaClass;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeMap;
import org.dom4j.tree.DefaultElement;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.internal.reference.DefaultStringDocumentReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class BaseClass_ESTest extends BaseClass_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      BaseClass baseClass0 = new BaseClass();
      String string0 = "";
      baseClass0.addLevelsField("", "", (-1217));
      BaseClass baseClass1 = baseClass0.clone();
      baseClass0.getIntValue("");
      TreeMap<String, DefaultStringDocumentReferenceResolver> treeMap0 = new TreeMap<String, DefaultStringDocumentReferenceResolver>();
      baseClass1.fromMap((Map<String, ?>) treeMap0, (BaseCollection) baseClass0);
      LinkedList<EmailMetaClass> linkedList0 = new LinkedList<EmailMetaClass>();
      baseClass1.setFieldsToRemove(linkedList0);
      baseClass0.setLongValue("", 2000L);
      DefaultStringDocumentReferenceResolver defaultStringDocumentReferenceResolver0 = new DefaultStringDocumentReferenceResolver();
      EntityReferenceResolver<DefaultElement> entityReferenceResolver0 = (EntityReferenceResolver<DefaultElement>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(defaultStringDocumentReferenceResolver0, (Class<?>) DefaultStringDocumentReferenceResolver.class, "entityReferenceResolver", (Object) entityReferenceResolver0);
      Injector.validateBean(defaultStringDocumentReferenceResolver0, (Class<?>) DefaultStringDocumentReferenceResolver.class);
      treeMap0.put("", defaultStringDocumentReferenceResolver0);
      baseClass1.clone();
      TreeMap<String, DefaultStringDocumentReferenceResolver> treeMap1 = new TreeMap<String, DefaultStringDocumentReferenceResolver>();
      // Undeclared exception!
      baseClass1.fromMap((Map<String, ?>) treeMap0, (BaseCollection) baseClass0);
  }
}
