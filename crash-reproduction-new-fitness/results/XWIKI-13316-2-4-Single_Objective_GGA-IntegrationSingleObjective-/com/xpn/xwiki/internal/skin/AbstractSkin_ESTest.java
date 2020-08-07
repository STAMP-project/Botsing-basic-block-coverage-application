/*
 * This file was automatically generated by EvoSuite
 * Sat May 16 17:20:54 UTC 2020
 */

package com.xpn.xwiki.internal.skin;

import org.junit.Test;
import static org.junit.Assert.*;
import static org.evosuite.shaded.org.mockito.Mockito.*;
import static org.evosuite.runtime.EvoAssertions.*;
import ch.qos.logback.classic.Logger;
import com.xpn.xwiki.internal.XWikiContextProvider;
import com.xpn.xwiki.internal.skin.EnvironmentSkin;
import com.xpn.xwiki.internal.skin.InternalSkinConfiguration;
import com.xpn.xwiki.internal.skin.InternalSkinManager;
import com.xpn.xwiki.util.XWikiStubContextProvider;
import org.evosuite.runtime.EvoRunner;
import org.evosuite.runtime.EvoRunnerParameters;
import org.evosuite.runtime.ViolatedAssumptionAnswer;
import org.evosuite.runtime.javaee.injection.Injector;
import org.hibernate.loader.custom.sql.SQLCustomQuery;
import org.junit.runner.RunWith;
import org.slf4j.helpers.NOPLogger;
import org.xwiki.context.Execution;
import org.xwiki.context.ExecutionContext;
import org.xwiki.environment.Environment;
import org.xwiki.rendering.internal.syntax.DefaultSyntaxFactory;

@RunWith(EvoRunner.class) @EvoRunnerParameters(useVFS = true, useJEE = true) 
public class AbstractSkin_ESTest extends AbstractSkin_ESTest_scaffolding {

  @Test(timeout = 4000)
  public void test0()  throws Throwable  {
      InternalSkinConfiguration internalSkinConfiguration0 = mock(InternalSkinConfiguration.class, new ViolatedAssumptionAnswer());
      Logger logger0 = (Logger)SQLCustomQuery.log;
      InternalSkinManager internalSkinManager0 = mock(InternalSkinManager.class, new ViolatedAssumptionAnswer());
      InternalSkinConfiguration internalSkinConfiguration1 = mock(InternalSkinConfiguration.class, new ViolatedAssumptionAnswer());
      org.slf4j.Logger logger1 = mock(org.slf4j.Logger.class, new ViolatedAssumptionAnswer());
      DefaultSyntaxFactory defaultSyntaxFactory0 = new DefaultSyntaxFactory();
      InternalSkinManager internalSkinManager1 = mock(InternalSkinManager.class, new ViolatedAssumptionAnswer());
      InternalSkinConfiguration internalSkinConfiguration2 = new InternalSkinConfiguration();
      org.slf4j.Logger logger2 = mock(org.slf4j.Logger.class, new ViolatedAssumptionAnswer());
      DefaultSyntaxFactory defaultSyntaxFactory1 = new DefaultSyntaxFactory();
      DefaultSyntaxFactory defaultSyntaxFactory2 = new DefaultSyntaxFactory();
      NOPLogger nOPLogger0 = NOPLogger.NOP_LOGGER;
      XWikiContextProvider xWikiContextProvider0 = new XWikiContextProvider();
      XWikiStubContextProvider xWikiStubContextProvider0 = mock(XWikiStubContextProvider.class, new ViolatedAssumptionAnswer());
      Injector.inject(xWikiContextProvider0, (Class<?>) XWikiContextProvider.class, "contextProvider", (Object) xWikiStubContextProvider0);
      Execution execution0 = mock(Execution.class, new ViolatedAssumptionAnswer());
      doReturn((ExecutionContext) null).when(execution0).getContext();
      Injector.inject(xWikiContextProvider0, (Class<?>) XWikiContextProvider.class, "execution", (Object) execution0);
      Injector.validateBean(xWikiContextProvider0, (Class<?>) XWikiContextProvider.class);
      xWikiContextProvider0.get();
      EnvironmentSkin environmentSkin0 = new EnvironmentSkin("O|", internalSkinManager1, internalSkinConfiguration2, logger0, defaultSyntaxFactory2, (Environment) null, xWikiContextProvider0);
      environmentSkin0.createResource("O|", "=j66J01*lO<r>C[A*");
      // Undeclared exception!
      environmentSkin0.getOutputSyntax();
  }
}
