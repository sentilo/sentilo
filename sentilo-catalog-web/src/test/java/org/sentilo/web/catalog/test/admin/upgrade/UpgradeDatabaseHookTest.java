package org.sentilo.web.catalog.test.admin.upgrade;

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Date;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.sentilo.common.test.AbstractBaseTest;
import org.sentilo.web.catalog.admin.upgrade.UpgradeDatabaseHook;
import org.sentilo.web.catalog.admin.upgrade.UpgradeDatabaseHook.ItemState;
import org.sentilo.web.catalog.admin.upgrade.UpgradeDatabaseItem;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.security.crypto.SentiloDelegatingPasswordEncoder;
import org.springframework.data.mongodb.core.MongoTemplate;

import com.mongodb.MongoException;

public class UpgradeDatabaseHookTest extends AbstractBaseTest {

  public final static String COLLECTION_NAME = "sentiloUpgradeControl";

  @InjectMocks
  private UpgradeDatabaseHook hook;

  @Mock
  private MongoTemplate mongoTemplate;

  @Mock
  private SentiloDelegatingPasswordEncoder passwordEncoder;

  @Before
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void afterPropertiesSet_whenCollectionNotExist() throws Exception {
    when(mongoTemplate.collectionExists(COLLECTION_NAME)).thenReturn(false);
    when(passwordEncoder.encodePasswords()).thenReturn(false);

    hook.afterPropertiesSet();

    verify(mongoTemplate).createCollection(COLLECTION_NAME);
  }

  @Test
  public void afterPropertiesSet_whenStateIsFinished() throws Exception {
    final String key = "encode_user_passwords.v18.sentilo";
    final UpgradeDatabaseItem upgradeState = new UpgradeDatabaseItem(key, ItemState.FINISHED, new Date());
    when(mongoTemplate.collectionExists(COLLECTION_NAME)).thenReturn(true);
    when(passwordEncoder.encodePasswords()).thenReturn(true);
    when(mongoTemplate.findById(key, UpgradeDatabaseItem.class, COLLECTION_NAME)).thenReturn(upgradeState);
    when(mongoTemplate.findById(key, UpgradeDatabaseItem.class, COLLECTION_NAME)).thenReturn(upgradeState);

    hook.afterPropertiesSet();

    verify(mongoTemplate, times(0)).createCollection(COLLECTION_NAME);
    verify(mongoTemplate, times(0)).save(any(UpgradeDatabaseItem.class));
  }

  @Test
  public void afterPropertiesSet_whenStateIsRunning() throws Exception {
    final String key = "encode_user_passwords.v18.sentilo";
    final UpgradeDatabaseItem upgradeState = new UpgradeDatabaseItem(key, ItemState.RUNNING, new Date());
    when(mongoTemplate.collectionExists(COLLECTION_NAME)).thenReturn(true);
    when(passwordEncoder.encodePasswords()).thenReturn(true);
    when(mongoTemplate.findById(key, UpgradeDatabaseItem.class, COLLECTION_NAME)).thenReturn(upgradeState);

    hook.afterPropertiesSet();

    verify(mongoTemplate, times(0)).createCollection(COLLECTION_NAME);
    verify(mongoTemplate, times(0)).save(any(UpgradeDatabaseItem.class));
  }

  @Test
  public void afterPropertiesSet() throws Exception {
    final String key = "encode_user_passwords.v18.sentilo";
    final String hash_pw = "$2a$12$tt9XjU0.9XW/IrknWUWLXuIPdN8IiFh8qnV//bsR7ftAugtQV9im2";
    final UpgradeDatabaseItem upgradeState = new UpgradeDatabaseItem(key);
    final User user_1 = Mockito.mock(User.class);
    final User user_2 = Mockito.mock(User.class);
    final List<User> users = Arrays.asList(user_1, user_2);
    when(mongoTemplate.collectionExists(COLLECTION_NAME)).thenReturn(true);
    when(passwordEncoder.encodePasswords()).thenReturn(true);
    when(mongoTemplate.findById(key, UpgradeDatabaseItem.class, COLLECTION_NAME)).thenReturn(upgradeState);
    when(mongoTemplate.findAll(User.class)).thenReturn(users);
    when(user_1.getPassword()).thenReturn("1234");
    when(user_2.getPassword()).thenReturn(hash_pw);
    when(passwordEncoder.encode("1234")).thenReturn(hash_pw);

    hook.afterPropertiesSet();

    verify(mongoTemplate, times(0)).createCollection(COLLECTION_NAME);
    verify(mongoTemplate, times(2)).save(any(UpgradeDatabaseItem.class), eq(COLLECTION_NAME));
    verify(user_1).setPassword(hash_pw);
    verify(mongoTemplate).save(user_1);
    verify(mongoTemplate, times(0)).save(user_2);
  }

  @Test(expected = MongoException.class)
  public void afterPropertiesSet_withError() throws Exception {
    final String key = "encode_user_passwords.v18.sentilo";
    final UpgradeDatabaseItem upgradeState = new UpgradeDatabaseItem(key);
    when(mongoTemplate.collectionExists(COLLECTION_NAME)).thenReturn(true);
    when(passwordEncoder.encodePasswords()).thenReturn(true);
    when(mongoTemplate.findById(key, UpgradeDatabaseItem.class, COLLECTION_NAME)).thenReturn(upgradeState);
    doThrow(new MongoException("Connection timed-out")).when(mongoTemplate).findAll(User.class);

    hook.afterPropertiesSet();

    verify(mongoTemplate, times(0)).createCollection(COLLECTION_NAME);
    verify(mongoTemplate, times(2)).save(any(UpgradeDatabaseItem.class), eq(COLLECTION_NAME));
    verify(mongoTemplate, times(0)).save(any(User.class));
  }
}
