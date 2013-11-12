/*
 * Sentilo
 *   
 * Copyright (C) 2013 Institut Municipal d’Informàtica, Ajuntament de  Barcelona.
 *   
 * This program is licensed and may be used, modified and redistributed under the
 * terms  of the European Public License (EUPL), either version 1.1 or (at your 
 * option) any later version as soon as they are approved by the European 
 * Commission.
 *   
 * Alternatively, you may redistribute and/or modify this program under the terms
 * of the GNU Lesser General Public License as published by the Free Software 
 * Foundation; either  version 3 of the License, or (at your option) any later 
 * version. 
 *   
 * Unless required by applicable law or agreed to in writing, software distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR 
 * CONDITIONS OF ANY KIND, either express or implied. 
 *   
 * See the licenses for the specific language governing permissions, limitations 
 * and more details.
 *   
 * You should have received a copy of the EUPL1.1 and the LGPLv3 licenses along 
 * with this program; if not, you may find them at: 
 *   
 *   https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
 *   http://www.gnu.org/licenses/ 
 *   and 
 *   https://www.gnu.org/licenses/lgpl.txt
 */
package org.sentilo.web.catalog.integration.service;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.sentilo.web.catalog.domain.User;
import org.sentilo.web.catalog.search.SearchFilter;
import org.sentilo.web.catalog.security.Role;
import org.sentilo.web.catalog.service.UserService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoOperations;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="classpath:spring/test-mongodb-service-context.xml")
public class UserServiceIntegrationTest {
	
	static final String ADMIN_ID="dummyAdmin";
	static final String USER1_ID="dummyUser1";
	static final String USER2_ID="dummyUser2";
	static final String USER3_ID="dummyUser3";
	static final String USER4_ID="dummyUser4";
	static final String USER5_ID="dummyUser5";
	static final String USER6_ID="dummyUser6";
	static final String USER7_ID="dummyUser7";
	static final String USER8_ID="dummyUser8";
	int initialUsersSize = 0;
	
	@Autowired
	private UserService userService;
		
	@Autowired
	MongoOperations mongoOps;		
	
	@Before
	public void onBefore(){
		//dropUsers();
	}
	
	@After
	public void onAfter(){		
		//dropUsers();
	}
	
	@Test
	public void doTest(){
		initialUsersSize  = userService.findAll().size();
		//create();
		//checkIntetgrity();
		//findAll();
		//searchWithFilters();
		searchWithPageableAndFilters();
//		searchWithPageable();
//		update();
		//delete();
	}

	
	public void create(){
		User user1 = new User(ADMIN_ID);		
		user1.setName("dummyAdmin");
		user1.setPassword("678910");
		user1.setActive(true);
		user1.setEmail("admin@perikolandia.com");
		addRole(Role.ADMIN, user1);									
		
		userService.create(user1);
		userService.create(buildDummyUser(USER1_ID));
		userService.create(buildDummyUser(USER2_ID));
		userService.create(buildDummyUser(USER3_ID));
		userService.create(buildDummyUser(USER4_ID));
		userService.create(buildDummyUser(USER5_ID));
		userService.create(buildDummyUser(USER6_ID));
		userService.create(buildDummyUser(USER7_ID));
		userService.create(buildDummyUser(USER8_ID));		
				
		assertTrue(userService.find(user1)!=null);
		assertTrue(userService.find(new User(USER1_ID))!=null);		
		assertTrue(userService.find(new User(USER2_ID))!=null);
		assertTrue(userService.find(new User(USER3_ID))!=null);
		assertTrue(userService.find(new User(USER4_ID))!=null);
		assertTrue(userService.find(new User(USER5_ID))!=null);
		assertTrue(userService.find(new User(USER6_ID))!=null);
		assertTrue(userService.find(new User(USER7_ID))!=null);
		assertTrue(userService.find(new User(USER8_ID))!=null);
	}
	
	
	public void checkIntetgrity(){
		boolean error = false;
		User user1 = new User(ADMIN_ID);		
		user1.setName("Administrador2");
		user1.setPassword("678910");
		user1.setActive(true);
		user1.setEmail("admin2@perikolandia.com");
		addRole(Role.ADMIN, user1);
		
		try{
			userService.create(user1);
		}catch(DuplicateKeyException dke){
			error = true;
		}
		
		assertTrue(error);		
	}
	
	
	public void findAll(){		
		List<User> users = userService.findAll();		
		assertTrue("Found " + users.size() + " and must be " + (initialUsersSize + 9), users.size() == (initialUsersSize + 9));
	}
	
	public void searchWithFilters(){				
		Map<String, String> params = new HashMap<String, String>();
		SearchFilter filter = new SearchFilter(params);
		params.put("name", "dummyAdmin");
		params.put("password", "678910");
		
		List<User> users = userService.search(filter).getContent();	
		assertTrue("Found " + users.size() + " and must be 1", users.size() == 1);
	}
	
	public void searchWithPageableAndFilters(){				
		final int size = 3;
		Pageable pageable = new PageRequest(1, size, new Sort("email"));		
		Map<String, String> params = new HashMap<String, String>();
		SearchFilter filter = new SearchFilter(params, pageable);
		params.put("name", "Usuario");
		params.put("password", "12345");
		
		List<User> users = userService.search(filter).getContent();	
		assertTrue("Found " + users.size() + " and must be " + size, users.size() == size);
	}
	
	public void searchWithPageable(){				
		final int size = 3;
		Pageable pageable = new PageRequest(1, size);		
		SearchFilter filter = new SearchFilter(pageable);		
		
		List<User> users = userService.search(filter).getContent();	
		assertTrue("Found " + users.size() + " and must be " + size, users.size() == size);
	}
	
	
	public void update(){
		String newName = "Perikillo";
		User user1 = userService.find(new User(ADMIN_ID));						
		assertEquals(user1.getName(), "dummyAdmin");		
		user1.setName(newName);
		userService.update(user1);
		User user2 = userService.find(new User(ADMIN_ID));
		assertEquals(user2.getName(), newName);
	}
	
	
	public void delete(){		
		userService.delete(new User(USER1_ID));
		User user2 = userService.find(new User(USER1_ID));
		assertTrue(user2 == null);
	}
	
	private User buildDummyUser(String id){
		User user = new User(id);		
		user.setName("dummyUsuario " + id);
		user.setPassword("12345");
		user.setActive(true);
		user.setEmail(id+"@perikolandia.com");
		addRole(Role.USER, user);
		
		return user;
	}
	
	@SuppressWarnings("unused")
	private void dropUsers() {
		User user1 = new User(ADMIN_ID);
		User user2 = new User(USER1_ID);
		User user3 = new User(USER2_ID);
		User user4 = new User(USER3_ID);
		User user5 = new User(USER4_ID);
		User user6 = new User(USER5_ID);
		User user7 = new User(USER6_ID);
		User user8 = new User(USER7_ID);
		User user9 = new User(USER8_ID);
		
		List<User> users = new ArrayList<User>();
		users.add(user1);
		users.add(user2);
		users.add(user3);
		users.add(user4);
		users.add(user5);
		users.add(user6);
		users.add(user7);
		users.add(user8);
		users.add(user9);
		userService.delete(users);	
	}
	
	private void addRole(Role role, User user){
		List<Role> rols = new ArrayList<Role>();
		rols.add(role);
		user.setRoles(rols);
	}			
						
}
