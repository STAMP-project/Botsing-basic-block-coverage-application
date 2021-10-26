/*
 * This file was automatically generated by EvoSuite
 * Tue Oct 26 05:59:31 UTC 2021
 */

package com.xpn.xwiki.store.hibernate.query;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import com.xpn.xwiki.store.hibernate.HibernateSessionFactory;
import com.xpn.xwiki.store.hibernate.query.HqlQueryExecutor;
import java.util.Map;
import javax.inject.Provider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.FlushMode;
import org.hibernate.Query;
import org.hibernate.engine.NamedSQLQueryDefinition;
import org.hibernate.engine.SessionImplementor;
import org.hibernate.engine.query.OrdinalParameterDescriptor;
import org.hibernate.engine.query.ParameterMetadata;
import org.hibernate.impl.QueryImpl;
import org.junit.runner.RunWith;
import org.slf4j.event.SubstituteLoggingEvent;
import org.slf4j.helpers.NOPLogger;
import org.xwiki.context.Execution;
import org.xwiki.job.event.status.JobProgressManager;
import org.xwiki.query.internal.LanguageQueryFilter;
import org.xwiki.query.internal.UniqueDocumentFilter;
import org.xwiki.security.authorization.ContextualAuthorizationManager;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class HqlQueryExecutor_ESTest extends HqlQueryExecutor_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      HqlQueryExecutor hqlQueryExecutor0 = new HqlQueryExecutor();
      ContextualAuthorizationManager contextualAuthorizationManager0 = mock(ContextualAuthorizationManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "authorization", (Object) contextualAuthorizationManager0);
      Provider<NamedSQLQueryDefinition> provider0 = (Provider<NamedSQLQueryDefinition>) mock(Provider.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "componentManagerProvider", (Object) provider0);
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "execution", (Object) execution0);
      JobProgressManager jobProgressManager0 = mock(JobProgressManager.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "progress", (Object) jobProgressManager0);
      HibernateSessionFactory hibernateSessionFactory0 = mock(HibernateSessionFactory.class, new ViolatedAssumptionAnswer());
      Injector.inject(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class, "sessionFactory", (Object) hibernateSessionFactory0);
      Injector.validateBean(hqlQueryExecutor0, (Class<?>) HqlQueryExecutor.class);
      Query query0 = mock(Query.class, new ViolatedAssumptionAnswer());
      doReturn((Query) null).when(query0).setParameter(anyString() , any());
      UniqueDocumentFilter uniqueDocumentFilter0 = new UniqueDocumentFilter();
      NOPLogger nOPLogger0 = NOPLogger.NOP_LOGGER;
      Injector.inject(uniqueDocumentFilter0, (Class<?>) UniqueDocumentFilter.class, "logger", (Object) nOPLogger0);
      Injector.validateBean(uniqueDocumentFilter0, (Class<?>) UniqueDocumentFilter.class);
      hqlQueryExecutor0.setNamedParameter(query0, "1Y$.^*Pe}LdusVRpO", uniqueDocumentFilter0);
      org.xwiki.query.Query query1 = mock(org.xwiki.query.Query.class, new ViolatedAssumptionAnswer());
      hqlQueryExecutor0.checkAllowed(query1);
      SubstituteLoggingEvent substituteLoggingEvent0 = new SubstituteLoggingEvent();
      HqlQueryExecutor.isSafeSelect("bRK@##J\u0000$Gh_6%324");
      LanguageQueryFilter languageQueryFilter0 = new LanguageQueryFilter();
      FlushMode flushMode0 = FlushMode.NEVER;
      SessionImplementor sessionImplementor0 = mock(SessionImplementor.class, new ViolatedAssumptionAnswer());
      OrdinalParameterDescriptor[] ordinalParameterDescriptorArray0 = new OrdinalParameterDescriptor[0];
      ParameterMetadata parameterMetadata0 = new ParameterMetadata(ordinalParameterDescriptorArray0, (Map) null);
      QueryImpl queryImpl0 = new QueryImpl("bRK@##J\u0000$Gh_6%324", flushMode0, sessionImplementor0, parameterMetadata0);
      // Undeclared exception!
      hqlQueryExecutor0.setNamedParameter(queryImpl0, "s@O/r", nOPLogger0);
  }
}
