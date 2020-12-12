/*
 * Sentilo
 *
 * Original version 1.4 Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de
 * Barcelona. Modified by Opentrends adding support for multitenant deployments and SaaS.
 * Modifications on version 1.5 Copyright (C) 2015 Opentrends Solucions i Sistemes, S.L.
 *
 *
 * This program is licensed and may be used, modified and redistributed under the terms of the
 * European Public License (EUPL), either version 1.1 or (at your option) any later version as soon
 * as they are approved by the European Commission.
 *
 * Alternatively, you may redistribute and/or modify this program under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation; either version 3 of the
 * License, or (at your option) any later version.
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied.
 *
 * See the licenses for the specific language governing permissions, limitations and more details.
 *
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along with this program;
 * if not, you may find them at:
 *
 * https://joinup.ec.europa.eu/software/page/eupl/licence-eupl http://www.gnu.org/licenses/ and
 * https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

import javax.annotation.PostConstruct;

import org.bson.Document;
import org.sentilo.web.catalog.admin.domain.DeletedResource;
import org.sentilo.web.catalog.domain.CatalogDocument;
import org.sentilo.web.catalog.domain.SyncResource;
import org.sentilo.web.catalog.exception.builder.DefaultCatalogBuilderExceptionImpl;
import org.sentilo.web.catalog.exception.builder.ResourceNotFoundExceptionBuilder;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.search.SearchFilterResult;
import org.sentilo.web.catalog.security.audit.Auditable;
import org.sentilo.web.catalog.security.audit.AuditingActionType;
import org.sentilo.web.catalog.security.service.CatalogUserDetailsService;
import org.sentilo.web.catalog.service.CrudService;
import org.sentilo.web.catalog.utils.Constants;
import org.sentilo.web.catalog.utils.TenantUtils;
import org.sentilo.web.catalog.validator.DefaultResourceKeyValidatorImpl;
import org.sentilo.web.catalog.validator.ResourceKeyValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.dao.DataAccessException;
import org.springframework.data.mongodb.core.BulkOperations;
import org.springframework.data.mongodb.core.BulkOperations.BulkMode;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.mongodb.MongoException;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.DistinctIterable;
import com.mongodb.client.MongoCollection;

public abstract class AbstractBaseCrudServiceImpl<T extends CatalogDocument> extends AbstractBaseServiceImpl implements CrudService<T>, ApplicationContextAware {

  protected static final Logger LOGGER = LoggerFactory.getLogger(AbstractBaseCrudServiceImpl.class);

  @Autowired
  private MongoOperations mongoOps;

  @Autowired
  protected CatalogUserDetailsService userDetailsService;

  private ApplicationContext context;

  private final Class<T> type;

  private ResourceKeyValidator resourceKeyValidator;
  private ResourceNotFoundExceptionBuilder resourceNotFoundExceptionBuilder;

  public AbstractBaseCrudServiceImpl(final Class<T> type) {
    this.type = type;
  }

  @PostConstruct
  public void init() {
    this.setResourceKeyValidator(new DefaultResourceKeyValidatorImpl(type, getRepository()));
    this.setResourceNotFoundExceptionBuilder(new DefaultCatalogBuilderExceptionImpl(type));
    doAfterInit();
  }

  public abstract MongoRepository<T, String> getRepository();

  public abstract String getEntityId(T entity);

  /*
   * (non-Javadoc)
   *
   * @see
   * org.springframework.context.ApplicationContextAware#setApplicationContext(org.springframework
   * .context.ApplicationContext)
   */
  @Override
  public void setApplicationContext(final ApplicationContext applicationContext) {
    context = applicationContext;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#create(java.lang.Object)
   */
  @Override
  @Auditable(actionType = AuditingActionType.CREATE)
  public T create(final T entity) {
    doBeforeCreate(entity);
    checkIntegrityKey(entity.getId());
    final T savedObject = getRepository().save(entity);
    doAfterCreate(entity);
    return savedObject;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#update(java.lang.Object)
   */
  @Override
  @Auditable(actionType = AuditingActionType.UPDATE)
  public T update(final T entity) {
    return getRepository().save(entity);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#insertAll(java.util.Collection)
   */
  @Override
  @Auditable(actionType = AuditingActionType.CREATE)
  public Collection<T> insertAll(final Collection<T> entities) throws MongoException, DataAccessException {

    if (!CollectionUtils.isEmpty(entities)) {
      final String collectionName = getMongoOps().getCollectionName(type);
      final BulkOperations bulkInsertOperation = getMongoOps().bulkOps(BulkMode.UNORDERED, this.type);
      for (final T entity : entities) {
        // Bulk operations are executed directly in MongoDB server so resource listeners registered
        // in the application are not invoked
        // (i.e. ComponentventListener or SensorEventListener).
        // To force these listeners execution a new BeforeConvertEvent is published to simulate the
        // same context defined in MongoTemplate#doSave/doInsert
        context.publishEvent(new BeforeConvertEvent<T>(entity, collectionName));
        bulkInsertOperation.insert(entity);
      }

      final BulkWriteResult result = bulkInsertOperation.execute();
      LOGGER.debug("{} resources of type {} has been inserted", result.getInsertedCount(), this.type.getName());
      doAfterCreate(entities);
    }

    return entities;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#updateAll(java.util.Collection)
   */
  @Override
  @Auditable(actionType = AuditingActionType.UPDATE)
  public Collection<T> updateAll(final Collection<T> entities) {
    // En el driver de Mongo no existe un update masivo de toda una coleccion, como si que existe el
    // insert masivo.
    // Por lo tanto, procedemos elemento a elemento. En caso de error en alguna actualizacion, se
    // marca el error y se prosigue con el update de los siguientes.
    final Collection<T> entitiesUpdated = new ArrayList<T>();
    if (!entities.isEmpty()) {
      for (final T entity : entities) {
        try {
          update(entity);
          entitiesUpdated.add(entity);
        } catch (final DataAccessException dae) {
          LOGGER.warn("Error updating entity with id {}", entity.getId());
        }
      }
    }

    return entitiesUpdated;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#delete(java.lang.Object)
   */
  @Override
  @Auditable(actionType = AuditingActionType.DELETE)
  public void delete(final T entity) {
    // getRepository().delete(entity);
    doDelete(entity);
    doAfterDelete(entity);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#delete(java.util.Collection)
   */
  @Override
  @Auditable(actionType = AuditingActionType.DELETE)
  public void delete(final Collection<T> entities) {
    // getRepository().delete(entities);
    doDelete(entities);
    doAfterDelete(entities);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.CrudService#delete(org.sentilo.web.catalog.search.SearchFilter)
   */
  @Override
  public void delete(final SearchFilter filter) {
    delete(filter, this.type);
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.CrudService#delete(org.sentilo.web.catalog.search.SearchFilter,
   * java.lang.Class)
   */
  @Override
  public <V extends CatalogDocument> void delete(final SearchFilter filter, final Class<V> resourceType) {
    final Query query = buildQuery(filter);
    doDelete(query, resourceType);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#find(java.lang.Object)
   */
  @Override
  public T find(final T entity) {
    // TODO: to redefine in a future release. Method should return an Optional<T> and clients should
    // control when result is either present or empty.
    // Actual version checks internally this control and returns null if result is not present to
    // facilitate the upgrade to new version of Spring Data without change many code of Sentilo.
    return findById(getEntityId(entity));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#findById(java.lang.String)
   */
  public T findById(final String id) {
    // TODO: deprecated find in a futures release and replace by this method
    final Optional<T> result = getRepository().findById(id);
    return result.isPresent() ? result.get() : null;
  }

  @Override
  public T findAndThrowErrorIfNotExist(final T entity) {
    final Optional<T> entityToReturn = getRepository().findById(entity.getId());
    if (!entityToReturn.isPresent()) {
      getResourceNotFoundExceptionBuilder().buildAndThrowResourceNotFoundException(entity.getId());
    }
    return entityToReturn.get();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#findAll()
   */
  @Override
  public List<T> findAll() {

    /*
     * // Get the sort field name value Sort sort = null; String fieldOrderName = null; try { // The
     * getOrderByFieldName() method always returns the search orderBy field name fieldOrderName =
     * this.type.newInstance().getOrderByFieldValue(); sort = new Sort(Direction.ASC,
     * fieldOrderName); } catch (final Exception e) { // On error... // Do nothing, and the sort
     * will be not applied }
     */

    if (applyFilterByTenant(this.type)) {
      final SearchFilter filter = new SearchFilter();
      filter.addAndParam("tenantId", TenantUtils.getCurrentTenant());
      return mongoOps.find(buildQuery(filter), this.type);
    } else {
      return getRepository().findAll();
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see
   * org.sentilo.web.catalog.service.CrudService#search(org.sentilo.web.catalog.utils.SearchFilter)
   */
  @Override
  public SearchFilterResult<T> search(final SearchFilter filter) {
    final Query countQuery = filter.isCountTotal() ? buildCountQuery(filter) : null;
    final long total = filter.isCountTotal() ? getMongoOps().count(countQuery, this.type) : -1;

    final Query query = buildQuery(filter);
    final List<T> content = getMongoOps().find(query, this.type);

    return new SearchFilterResult<T>(content, total);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#count()
   */
  @Override
  public Long count() {
    return getRepository().count();
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#exist(java.lang.String)
   */
  @Override
  public boolean exists(final String entityId) {
    return getRepository().existsById(entityId);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#updateMulti(java.util.Collection,
   * java.lang.String, java.lang.Object)
   */
  @Override
  public void updateMulti(final Collection<String> objectIds, final String param, final Object value) {
    updateMulti(objectIds, Arrays.asList(param), Arrays.asList(value));
  }

  /*
   * (non-Javadoc)
   *
   * @see org.sentilo.web.catalog.service.CrudService#updateMulti(java.util.Collection,
   * java.util.List, java.util.List)
   */
  @Override
  public <V> void updateMulti(final Collection<String> objectIds, final List<String> params, final List<V> values) {
    updateMulti(buildQueryForIdInCollection(objectIds), params, values);
  }

  public Class<T> getType() {
    return type;
  }

  protected <V> void updateMulti(final Query query, final List<String> params, final List<V> values) {
    Assert.isTrue(params.size() == values.size(), "[Assertion failed] - number of parameters and values must be equal");

    final String userName = userDetailsService.getCatalogUserDetails().getUsername();
    final Update update = Update.update(params.get(0), values.get(0));

    for (int i = 1; i < params.size(); i++) {
      update.set(params.get(i), values.get(i));
    }

    update.set("updatedAt", new Date()).set("updatedBy", userName);
    if (SyncResource.class.isAssignableFrom(type)) {
      update.set(Constants.SYNC_FIELD, null);
    }

    getMongoOps().updateMulti(query, update, type);
  }

  protected void doDelete(final T entity) {
    final List<String> ids = Arrays.asList(new String[] {entity.getId()});
    doDelete(buildQueryForIdInCollection(ids), this.type);
  }

  protected void doDelete(final Collection<T> entities) {
    final List<String> ids = new ArrayList<String>();
    for (final T entity : entities) {
      ids.add(entity.getId());
    }

    doDelete(buildQueryForIdInCollection(ids), this.type);
  }

  protected <V extends CatalogDocument> void doDelete(final Query query) {
    doDelete(query, this.type);
  }

  protected <V extends CatalogDocument> void doDelete(final Query query, final Class<V> resourceType) {
    if (SyncResource.class.isAssignableFrom(resourceType)) {

      final String collectionName = getMongoOps().getCollectionName(resourceType);
      final Query queryFiltered = query;
      queryFiltered.fields().include("sensorId").include("providerId").include("applicationId");
      final List<DeletedResource> resources = getMongoOps().find(queryFiltered, DeletedResource.class, collectionName);

      if (!CollectionUtils.isEmpty(resources)) {
        final BulkOperations bulkInsertOperation = getMongoOps().bulkOps(BulkMode.UNORDERED, DeletedResource.class);
        for (final DeletedResource resource : resources) {
          resource.setResourceClass(resourceType.getName());
          resource.setDeletedAt(new Date());
          bulkInsertOperation.insert(resource);
        }

        final BulkWriteResult result = bulkInsertOperation.execute();
        LOGGER.debug("{} deleted resources has been registered to be synchronized later ", result.getInsertedCount());
      }
    }

    getMongoOps().remove(query, resourceType);
  }

  protected List<String> distinct(final String collectionName, final String fieldName) {
    return distinct(collectionName, fieldName, String.class, null);
  }

  protected List<String> distinct(final String collectionName, final String fieldName, final Query query) {
    return distinct(collectionName, fieldName, String.class, query);
  }

  protected <TResult> List<TResult> distinct(final String collectionName, final String fieldName, final Class<TResult> resultClass,
      final Query query) {
    // MongoCollection, from version 3.0, has change distinct method signature
    // https://groups.google.com/forum/#!msg/mongodb-user/7YxevdAPcdY/vHwydGIkAQAJ
    // getMongoOps().getCollection(collectionName).distinct(fieldName, query.getQueryObject(),
    // String.class);

    final MongoCollection<Document> collection = getMongoOps().getCollection(collectionName);
    if (query != null) {
      return toList(collection.distinct(fieldName, query.getQueryObject(), resultClass));
    } else {
      return toList(collection.distinct(fieldName, resultClass));
    }
  }

  protected <V> List<V> toList(final DistinctIterable<V> result) {
    final List<V> list = new ArrayList<V>();
    final Iterator<V> it = result.iterator();
    while (it.hasNext()) {
      list.add(it.next());
    }
    return list;
  }

  protected void checkIntegrityKey(final String idToCheck) {
    getResourceKeyValidator().checkIntegrityKey(idToCheck);
  }

  protected void doAfterUpdateMulti(final Collection<String> objectIds, final String[] params, final Object[] values) {
    // To override by subclasses.
  }

  protected void doBeforeCreate(final T entity) {
    // To override by subclasses.
  }

  protected void doAfterDelete(final T entity) {
    // To override by subclasses.
  }

  protected void doAfterDelete(final Collection<T> entities) {
    // To override by subclasses.
  }

  protected void doAfterCreate(final T entity) {
    // To override by subclasses.
  }

  protected void doAfterCreate(final Collection<T> entities) {
    // To override by subclasses.
  }

  protected void doAfterInit() {
    // To override by subclasses
  }

  public MongoOperations getMongoOps() {
    return mongoOps;
  }

  protected ApplicationContext getContext() {
    return context;
  }

  protected ResourceKeyValidator getResourceKeyValidator() {
    return resourceKeyValidator;
  }

  protected void setResourceKeyValidator(final ResourceKeyValidator resourceKeyValidator) {
    this.resourceKeyValidator = resourceKeyValidator;
  }

  protected ResourceNotFoundExceptionBuilder getResourceNotFoundExceptionBuilder() {
    return resourceNotFoundExceptionBuilder;
  }

  protected void setResourceNotFoundExceptionBuilder(final ResourceNotFoundExceptionBuilder resourceNotFoundExceptionBuilder) {
    this.resourceNotFoundExceptionBuilder = resourceNotFoundExceptionBuilder;
  }

}
