/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 14:40:55 UTC 2020
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
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.reference.EntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      EntityReferenceResolver<DefaultStringEntityReferenceResolver> entityReferenceResolver0 = (EntityReferenceResolver<DefaultStringEntityReferenceResolver>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      SolrDocument solrDocument0 = new SolrDocument();
      EntityType entityType0 = EntityType.DOCUMENT;
      SolrDocument solrDocument1 = new SolrDocument();
      solrDocument0.put("wiki", (Object) solrDocument1);
      EntityType entityType1 = EntityType.BLOCK;
      Object object0 = new Object();
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      EntityReferenceResolver<Integer> entityReferenceResolver1 = (EntityReferenceResolver<Integer>) mock(EntityReferenceResolver.class, new ViolatedAssumptionAnswer());
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) entityReferenceResolver1);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      EntityType entityType2 = EntityType.ATTACHMENT;
      Object[] objectArray0 = new Object[5];
      objectArray0[0] = (Object) solrEntityReferenceResolver0;
      objectArray0[1] = (Object) entityType0;
      Object object1 = new Object();
      objectArray0[2] = object1;
      objectArray0[3] = (Object) "wiki";
      objectArray0[4] = (Object) entityType1;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType2, objectArray0);
  }
}
