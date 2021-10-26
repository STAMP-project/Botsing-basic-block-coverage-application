/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 06:04:36 UTC 2021
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.io.InputStream;
import javax.inject.Provider;
import org.apache.struts.chain.contexts.MockActionContext;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.FlushMode;
import org.hibernate.Query;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.CollectionFilterImpl;
import org.hibernate.loader.custom.sql.SQLCustomQuery;
import org.hibernate.type.PostgresUUIDType;
import org.junit.runner.RunWith;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.internal.CountDocumentFilter;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      CountDocumentFilter countDocumentFilter0 = new CountDocumentFilter();
      Logger logger0 = (Logger)SQLCustomQuery.log;
      Injector.inject(countDocumentFilter0, (Class<?>) CountDocumentFilter.class, "logger", (Object) logger0);
      Injector.validateBean(countDocumentFilter0, (Class<?>) CountDocumentFilter.class);
      HqlQueryExecutor.isSafeSelect("query.hql.progressmZxecute");
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      Provider<InputStream> provider0 = (Provider<InputStream>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider0);
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution0);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[3];
      PostgresUUIDType postgresUUIDType0 = PostgresUUIDType.INSTANCE;
      OrdinalParameterDescriptor ordinalParameterDescriptor0 = new OrdinalParameterDescriptor((-9), postgresUUIDType0, (-9));
      ordinalParameterDescriptorArray0[0] = ordinalParameterDescriptor0;
      OrdinalParameterDescriptor ordinalParameterDescriptor1 = new OrdinalParameterDescriptor((-9), postgresUUIDType0, (-9));
      ordinalParameterDescriptorArray0[1] = ordinalParameterDescriptor1;
      OrdinalParameterDescriptor ordinalParameterDescriptor2 = new OrdinalParameterDescriptor((-9), postgresUUIDType0, (-9));
      ordinalParameterDescriptorArray0[2] = ordinalParameterDescriptor2;
      MockActionContext mockActionContext0 = new MockActionContext();
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, mockActionContext0);
      CollectionFilterImpl collectionFilterImpl0 = new CollectionFilterImpl("query.hql.progressmZxecute", logger0, sessionImplementor0, parameterMetadata0);
      FlushMode flushMode0 = FlushMode.AUTO;
      Query query0 = collectionFilterImpl0.setFlushMode(flushMode0);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(query0, "query.hql.progressmZxecute", "yG>3o*Q");
  }
}
