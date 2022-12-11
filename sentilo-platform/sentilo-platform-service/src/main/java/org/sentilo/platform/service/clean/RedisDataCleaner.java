/*
 * Sentilo
 *  
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS. 
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 * 
 *   
 * This program is licensed and may be used, modified and redistributed under the terms  of the 
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon 
 * as they are approved by the European Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either  version 3 of the 
 * License, or (at your option) any later version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed under the License 
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR  CONDITIONS OF ANY KIND, either express 
 * or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program; 
 * if not, you may find them at: 
 *   
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/   and 
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.platform.service.clean;

import java.time.Duration;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import org.sentilo.common.enums.EventType;
import org.sentilo.common.utils.MessagingUtils;
import org.sentilo.platform.service.impl.AbstractPlatformServiceImpl;
import org.sentilo.platform.service.impl.ResourceServiceImpl;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.redis.core.Cursor;
import org.springframework.data.redis.core.RedisCallback;
import org.springframework.data.redis.core.ScanOptions;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.util.Pair;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;

/**
 * Utility responsible of run maintenance tasks over Redis data: clean all orphan ZSET entries from
 * Redis database and ensures that the events stream's length doesn't exceed a fixed threshold.
 *
 * <p>
 * This clean runs periodically in three scheduled tasks:
 * <ul>
 * <li>One task cleans all orphan entries detected by clients when execute read requests
 * {@link #clean}.
 * <li>Another task cleans all orphan entries with a score older than a given instant
 * {@link #scanAndClean}.
 * <li>Finally, a third task, that runs every 30 seconds (by default), ensures that events streams
 * always have a length lower than a given limit (set by configuration and with default value equals
 * 10000). When Redis client adds support to trim streams by MIN_ID, this process will trim streams
 * by timestamp and not by length.
 * </ul>
 * </p>
 *
 */
@Component
public class RedisDataCleaner extends AbstractPlatformServiceImpl {

  private static final Logger LOGGER = LoggerFactory.getLogger(RedisDataCleaner.class);
  private static final long DEFAULT_CHECK_BATCH_SIZE = 500;
  private static final long DEFAULT_CHECK_MAX_EVENTS = 10000;
  private static final int DEFAULT_DELETE_BATCH_SIZE = 200;
  private static final long DEFAULT_STREAMS_MESSAGES_TTL_MS = Duration.ofDays(1).toMillis();
  private static final long DEFAULT_STREAMS_MAX_LENGTH = 10000;

  @Autowired
  private StringRedisTemplate redisTemplate;
  @Value("${sentilo.server.api.redis-clean-task.scan_clean_db.check_batch_size:500}")
  private long check_batch_size = DEFAULT_CHECK_BATCH_SIZE;
  @Value("${sentilo.server.api.redis-clean-task.scan_clean_db.check_max_entries:10000}")
  private long check_max_events = DEFAULT_CHECK_MAX_EVENTS;
  @Value("${sentilo.server.api.redis-clean-task.scan_clean_db.delete_batch_size:200}")
  private int delete_batch_size = DEFAULT_DELETE_BATCH_SIZE;
  @Value("${sentilo.server.api.redis-clean-task.scan_clean_db.streams_messages_ttl_ms:86400000}")
  private long streams_messages_ttl_ms = DEFAULT_STREAMS_MESSAGES_TTL_MS;
  @Value("${sentilo.server.api.redis-clean-task.clean_streams.streams_max_length:10000}")
  private long streams_messages_max_length = DEFAULT_STREAMS_MAX_LENGTH;

  private final RedisSerializer<String> redisSerializer = RedisSerializer.string();

  /**
   * Map that holds pairs <zset_id, list of orphan events in zset_id> detected when a client
   * executes requests for read last events from a sensor.
   */
  private final Map<String, List<String>> orphanEvents = new ConcurrentHashMap<String, List<String>>();

  final Lock orphanLock = new ReentrantLock();

  public void addOrphanEventsToRemove(final EventType type, final String zset, final List<String> eventsIds) {
    orphanLock.lock();
    try {
      if (orphanEvents.containsKey(zset)) {
        orphanEvents.get(zset).addAll(eventsIds);
      } else {
        orphanEvents.put(zset, eventsIds);
      }
    } finally {
      orphanLock.unlock();
    }
  }

  /**
   * Removes older messages from events streams, i.e, maintains only the first
   * streams_messages_max_length messages.
   *
   */
  @Scheduled(initialDelay = 30000, fixedDelayString = "${sentilo.server.api.redis-clean-task.clean_streams.job.delay:30000}")
  public void trimStreams() {
    try {

      for (final EventType eventType : EventType.values()) {
        final String streamKey = MessagingUtils.getStreamName(eventType);
        if (redisTemplate.hasKey(streamKey) && redisTemplate.opsForStream().size(streamKey) > streams_messages_max_length) {
          final long count = redisTemplate.opsForStream().trim(streamKey, streams_messages_max_length, true);
          LOGGER.debug("Removed {} older messages from stream {}", count, streamKey);
        }
      }
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while removing streams' older messages.", e);
    }
  }

  /**
   * Remove ZSET orphan events which have been detected by clients while get last events.
   *
   * @see {@link ResourceServiceImpl#addOrphanEventsToRemove}
   */
  @Scheduled(initialDelay = 60000, fixedDelayString = "${sentilo.server.api.redis-clean-task.clean_orphan_events.job.delay:60000}")
  public void clean() {
    try {
      final Map<String, List<String>> orphanEventsCopy = cloneOrphanEvents();
      cleanOrphanEvents(orphanEventsCopy);
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while cleaning ZSET orphan elements.", e);
    }
  }

  /**
   * Scans all database and check state of each ZSET, removing all orphan elements older than a
   * given time.
   *
   * These process ensures that ZSETs no hold orphaned entries older than a given time and should be
   * run continuously so that's why <code>fixedDelayString</code> by default is 1s.
   */
  @Scheduled(initialDelay = 1000, fixedDelayString = "${sentilo.server.api.redis-clean-task.scan_clean_db.job.delay:1000}")
  public void scanAndClean() {
    final Instant init = Instant.now();
    try {
      LOGGER.debug("Init scanAndCleanDatabase process that cleans orphan entries from Redis ZSETs");
      scanAndCleanDatabase();
    } catch (final Exception e) {
      LOGGER.warn("An error has been raised while cleaning Redis database.", e);
    } finally {
      final Instant end = Instant.now();
      LOGGER.info("Duration of scanAndCleanDatabase process {} minutes", Duration.between(init, end).toMinutes());
    }
  }

  private Map<String, List<String>> cloneOrphanEvents() {
    orphanLock.lock();
    try {
      final Map<String, List<String>> target = new ConcurrentHashMap<String, List<String>>(orphanEvents);
      orphanEvents.clear();
      return target;
    } finally {
      orphanLock.unlock();
    }
  }

  private void cleanOrphanEvents(final Map<String, List<String>> zSetEventsToRemove) {
    // @formatter:off
    zSetEventsToRemove.keySet().stream().forEach(zsetId -> {
      // Remove zset orphan elements in groups of up ${delete_batch_size} elements to no penalize call to Redis and because the method has a time complexity of
      // O(M*log(N)) with N being the number of elements in the sorted set and M the number of elements to be removed
      long total = 0;
      final List<List<String>> sublists = Lists.partition(zSetEventsToRemove.get(zsetId), delete_batch_size);
      for (final List<String> sublist : sublists) {
        total += redisTemplate.opsForZSet().remove(zsetId, sublist);
      }

      LOGGER.info("Removed {} orphan elements from ZSET {}", total, zsetId);
    });
    // @formatter:on

  }

  /**
   * This method scan all ZSET entries from Redis and for each one remove entries with a score older
   * than either now()- sensor_ttl if ZSET contains references to data or orders or now
   * -instance_ttl if it contains references to alarms.
   */
  private void scanAndCleanDatabase() {
    scanAndCleanEventsZsets(Pair.of("sid:*:observations", "sdid:"));
    scanAndCleanEventsZsets(Pair.of("sid:*:orders", "soid:"));
    scanAndCleanEventsZsets(Pair.of("aid:*:alarms", "amid:"));
  }

  private void scanAndCleanEventsZsets(final Pair<String, String> contextScan) {
    final Instant init = Instant.now();

    LOGGER.debug("*****************************************************************************************************");
    LOGGER.debug("Init process for clean ZSETs of type {}", contextScan.getFirst());
    final AtomicInteger count = new AtomicInteger();
 // @formatter:off
    final long totalRemoved = redisTemplate.execute((RedisCallback<Long>) connection -> {
        long entriesRemoved = 0;
        final ScanOptions scanOptions = ScanOptions.scanOptions().match(contextScan.getFirst()).count(50).build();
        final Cursor<byte[]> c = connection.scan(scanOptions);
        while (c.hasNext()) {
          final String zsetId = redisSerializer.deserialize(c.next());
          entriesRemoved += scanAndCleanZset(zsetId, contextScan);
          count.incrementAndGet();
        }

        try {
          c.close();
        } catch (final Exception e) {
          // Nothing to do .. scan should continue
        }

        return entriesRemoved;
    });
 // @formatter:on

    final Instant end = Instant.now();
    LOGGER.debug("Finally checked {} ZSETs and removed {} orphan entries", count.get(), totalRemoved);
    LOGGER.debug("Duration of check process for ZSET of type {}: {} minutes", contextScan.getFirst(), Duration.between(init, end).toMinutes());
    LOGGER.debug("*****************************************************************************************************");
  }

  private long scanAndCleanZset(final String zsetId, final Pair<String, String> contextScan) {
    LOGGER.debug("-----------------------------------------------------------------------------------------------------");
    LOGGER.debug("Init clean of ZSET: {} ", zsetId);
    // Get the oldest entries from ZSET and check if associated events have expired and, in this
    // case, removing them.
    // For each ZSET only check entries with a score lower than [now - instance_ttl]
    final long toScore = System.currentTimeMillis() - TimeUnit.SECONDS.toMillis(expireSeconds);
    final long count = redisTemplate.opsForZSet().count(zsetId, 0, toScore);
    final LocalDateTime toScoreDate = LocalDateTime.ofInstant(Instant.ofEpochMilli(toScore), ZoneId.systemDefault());
    LOGGER.debug("Found {} entries older than {} in ZSET {}", count, toScoreDate, zsetId);
    final long total = count > check_max_events ? check_max_events : count;
    final long iterations = total / check_batch_size;
    final List<String> expiredEvents = new ArrayList<String>();

    for (int i = 0; i <= iterations; i++) {
      final long offset = i * check_batch_size;
      final Set<String> eventsIds = redisTemplate.opsForZSet().rangeByScore(zsetId, 0, toScore, offset, check_batch_size);
      expiredEvents
          .addAll(eventsIds.stream().filter(eventId -> !redisTemplate.hasKey(contextScan.getSecond() + eventId)).collect(Collectors.toList()));
    }

    LOGGER.debug("Read {} orphan entries from ZSET {}, which should be removed.", expiredEvents.size(), zsetId);

    long zsetEntriesRemoved = 0;
    if (!CollectionUtils.isEmpty(expiredEvents)) {
      final List<List<String>> subLists = Lists.partition(expiredEvents, delete_batch_size);
      for (final List<String> aux : subLists) {
        zsetEntriesRemoved += redisTemplate.opsForZSet().remove(zsetId, aux.toArray());
        LOGGER.debug("... removed {} entries from ZSET {} ... ", aux.size(), zsetId);
      }

      LOGGER.debug("Finally {} orphan entries has been removed from ZSET {}", zsetEntriesRemoved, zsetId);
    }
    LOGGER.debug("-----------------------------------------------------------------------------------------------------");

    return zsetEntriesRemoved;
  }

}
