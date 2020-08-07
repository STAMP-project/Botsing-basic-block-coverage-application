/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 14:44:54 UTC 2020
 */

package org.xwiki.search.solr.internal.reference;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.runtime.EvoAssertions.*;
import org.apache.solr.common.SolrDocument;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.javaee.injection.Injector;
import org.junit.runner.RunWith;
import org.xwiki.model.EntityType;
import org.xwiki.model.internal.reference.DefaultReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.DefaultStringEntityReferenceResolver;
import org.xwiki.model.internal.reference.ExplicitReferenceEntityReferenceResolver;
import org.xwiki.model.internal.reference.ExplicitStringEntityReferenceResolver;
import org.xwiki.search.solr.internal.reference.SolrEntityReferenceResolver;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class SolrEntityReferenceResolver_ESTest extends SolrEntityReferenceResolver_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      DefaultReferenceEntityReferenceResolver defaultReferenceEntityReferenceResolver0 = new DefaultReferenceEntityReferenceResolver();
      DefaultStringEntityReferenceResolver defaultStringEntityReferenceResolver0 = new DefaultStringEntityReferenceResolver();
      EntityType entityType0 = EntityType.OBJECT;
      SolrEntityReferenceResolver solrEntityReferenceResolver0 = new SolrEntityReferenceResolver();
      Injector.inject(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class, "explicitReferenceEntityReferenceResolver", (Object) defaultReferenceEntityReferenceResolver0);
      Injector.validateBean(solrEntityReferenceResolver0, (Class<?>) SolrEntityReferenceResolver.class);
      SolrDocument solrDocument0 = new SolrDocument();
      Object object0 = new Object();
      EntityType entityType1 = EntityType.DOCUMENT;
      ExplicitStringEntityReferenceResolver explicitStringEntityReferenceResolver0 = new ExplicitStringEntityReferenceResolver();
      ExplicitReferenceEntityReferenceResolver explicitReferenceEntityReferenceResolver0 = new ExplicitReferenceEntityReferenceResolver();
      solrDocument0.put("wiki", (Object) explicitReferenceEntityReferenceResolver0);
      Object object1 = new Object();
      Object[] objectArray0 = new Object[6];
      objectArray0[0] = object1;
      objectArray0[1] = (Object) "wiki";
      objectArray0[2] = (Object) entityType1;
      objectArray0[3] = object0;
      objectArray0[4] = (Object) solrDocument0;
      objectArray0[5] = (Object) entityType1;
      // Undeclared exception!
      solrEntityReferenceResolver0.resolve(solrDocument0, entityType0, objectArray0);
  }
}
