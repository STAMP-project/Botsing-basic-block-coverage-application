/*
 * This file was automatically generated by EvoSuite
 * Thu May 14 14:20:50 UTC 2020
 */

package org.xwiki.search.solr.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.solr.common.SolrDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.RelativeStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<DefaultReferenceEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<DefaultReferenceEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver0);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      SolrEntityReferenceResolver solrEntityReferenceResolver1 = new SolrEntityReferenceResolver();
      Injector.inject(solrEntityReferenceResolver1, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) solrEntityReferenceResolver0);
      Injector.validateBean(solrEntityReferenceResolver1, (Class<?>) SolrEntityReferenceResolver.class);
      SolrEntityReferenceResolver solrEntityReferenceResolver2 = new SolrEntityReferenceResolver();
      SolrDocument solrDocument0 = new SolrDocument();
      String string0 = "wiki";
      RelativeStringEntityReferenceResolver relativeStringEntityReferenceResolver0 = new RelativeStringEntityReferenceResolver();
      solrDocument0.put("wiki", (Object) relativeStringEntityReferenceResolver0);
      EntityType entityType0 = EntityType.CLASS_PROPERTY;
      Object[] objectArray0 = new Object[8];
      objectArray0[0] = (Object) solrEntityReferenceResolver2;
      objectArray0[1] = (Object) solrEntityReferenceResolver1;
      objectArray0[2] = (Object) solrDocument0;
      objectArray0[3] = (Object) entityType0;
      objectArray0[4] = (Object) solrDocument0;
      Object object0 = new Object();
      objectArray0[5] = object0;
      objectArray0[6] = (Object) entityType0;
      objectArray0[7] = (Object) solrEntityReferenceResolver0;
      // Undeclared exception!
      solrEntityReferenceResolver1.resolve(solrDocument0, entityType0, objectArray0);
  }
}
