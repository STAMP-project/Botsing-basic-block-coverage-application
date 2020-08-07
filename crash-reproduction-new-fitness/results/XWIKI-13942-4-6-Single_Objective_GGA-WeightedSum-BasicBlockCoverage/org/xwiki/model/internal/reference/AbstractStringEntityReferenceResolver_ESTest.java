/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 12:52:24 UTC 2020
 */

package org.xwiki.model.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.AbstractStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultSymbolScheme;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractStringEntityReferenceResolver_ESTest extends AbstractStringEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultSymbolScheme defaultSymbolScheme0 = new DefaultSymbolScheme();
      defaultSymbolScheme0.initialize();
      DefaultSymbolScheme defaultSymbolScheme1 = new DefaultSymbolScheme();
      defaultSymbolScheme1.initialize();
      EntityType entityType0 = EntityType.ATTACHMENT;
      defaultSymbolScheme1.initialize();
      defaultSymbolScheme0.initialize();
      Object object0 = new Object();
      defaultSymbolScheme1.initialize();
      defaultSymbolScheme0.initialize();
      EntityType entityType1 = EntityType.WIKI;
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      DefaultSymbolScheme defaultSymbolScheme2 = new DefaultSymbolScheme();
      Injector.inject(explicitStringEntityReferenceResolver0, (Class<?>) AbstractStringEntityReferenceResolver.class, "symbolScheme", (Object) defaultSymbolScheme2);
      Injector.validateBean(explicitStringEntityReferenceResolver0, (Class<?>) ExplicitStringEntityReferenceResolver.class);
      explicitStringEntityReferenceResolver0.initialize();
      EntityType entityType2 = EntityType.OBJECT_PROPERTY;
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) entityType0;
      objectArray0[1] = (Object) entityType2;
      objectArray0[2] = (Object) defaultSymbolScheme0;
      Object object1 = new Object();
      objectArray0[3] = object1;
      objectArray0[4] = (Object) "_'M%4p";
      // Undeclared exception!
      explicitStringEntityReferenceResolver0.resolve("_'M%4p", entityType2, objectArray0);
  }
}
