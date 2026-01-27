use std::collections::HashMap;
use std::sync::Arc;

use axum::Router;
use axum::extract::{Json, Path, Query, State};
use tokio::sync::RwLock;

// Module-based macro with split traits by responsibility
#[oxapi::oxapi(axum, "petstore.json")]
mod petstore {
    #[oxapi(post, "/pet", ok)]
    #[derive(Debug, Clone)]
    enum PetResponse {
        #[oxapi(status = 200)]
        Created,
    }

    #[oxapi("Pet")]
    struct Animal;

    // Example: Rename inline response type using variant syntax
    // The GetInventory response has an inline object schema, we rename it to "Inventory"
    #[oxapi(get, "/store/inventory", ok)]
    enum InventoryResponse {
        #[oxapi(status = 200)]
        Success(Inventory),
    }

    // Pet operations - generic over state type for dependency injection
    trait PetService<S: PetStateProvider> {
        #[oxapi(map)]
        fn map_routes(router: Router<S>) -> Router<S>;

        #[oxapi(get, "/pet/{petId}")]
        async fn get_pet_by_id(state: State<S>, pet_id: Path<_>);

        #[oxapi(post, "/pet")]
        async fn add_pet(state: State<S>, body: Json<_>);

        #[oxapi(put, "/pet")]
        async fn update_pet(state: State<S>, body: Json<_>);

        #[oxapi(delete, "/pet/{petId}")]
        async fn delete_pet(state: State<S>, pet_id: Path<_>);

        #[oxapi(get, "/pet/findByStatus")]
        async fn find_pets_by_status(state: State<S>, query: Query<_>);

        #[oxapi(get, "/pet/findByTags")]
        async fn find_pets_by_tags(state: State<S>, query: Query<_>);

        #[oxapi(post, "/pet/{petId}")]
        async fn update_pet_with_form(state: State<S>, pet_id: Path<_>, query: Query<_>);

        #[oxapi(post, "/pet/{petId}/uploadImage")]
        async fn upload_file(state: State<S>, pet_id: Path<_>, query: Query<_>);
    }

    // Store operations
    trait StoreService {
        #[oxapi(map)]
        fn map_routes(router: Router<StoreState>) -> Router<StoreState>;

        #[oxapi(get, "/store/inventory")]
        async fn get_inventory(state: State<StoreState>);

        #[oxapi(post, "/store/order")]
        async fn place_order(state: State<StoreState>, body: Json<_>);

        #[oxapi(get, "/store/order/{orderId}")]
        async fn get_order_by_id(state: State<StoreState>, order_id: Path<_>);

        #[oxapi(delete, "/store/order/{orderId}")]
        async fn delete_order(state: State<StoreState>, order_id: Path<_>);
    }

    // User operations
    trait UserService {
        #[oxapi(map)]
        fn map_routes(router: Router<UserState>) -> Router<UserState>;

        #[oxapi(post, "/user")]
        async fn create_user(state: State<UserState>, body: Json<_>);

        #[oxapi(post, "/user/createWithList")]
        async fn create_users_with_list_input(state: State<UserState>, body: Json<_>);

        #[oxapi(get, "/user/login")]
        async fn login_user(state: State<UserState>, query: Query<_>);

        #[oxapi(get, "/user/logout")]
        async fn logout_user(state: State<UserState>);

        #[oxapi(get, "/user/{username}")]
        async fn get_user_by_name(state: State<UserState>, username: Path<_>);

        #[oxapi(put, "/user/{username}")]
        async fn update_user(state: State<UserState>, username: Path<_>, body: Json<_>);

        #[oxapi(delete, "/user/{username}")]
        async fn delete_user(state: State<UserState>, username: Path<_>);
    }
}

// Import the generated types and traits
use petstore::types::*;
use petstore::{PetService, StoreService, UserService};

// Trait for dependency injection of PetService state
pub trait PetStateProvider: Clone + Send + Sync + 'static {
    fn pets(&self) -> &Arc<RwLock<HashMap<i64, Animal>>>;
    fn next_id(&self) -> &Arc<RwLock<i64>>;
}

// Separate state structs for each service
#[derive(Clone)]
pub struct PetState {
    pets: Arc<RwLock<HashMap<i64, Animal>>>,
    next_id: Arc<RwLock<i64>>,
}

impl PetStateProvider for PetState {
    fn pets(&self) -> &Arc<RwLock<HashMap<i64, Animal>>> {
        &self.pets
    }

    fn next_id(&self) -> &Arc<RwLock<i64>> {
        &self.next_id
    }
}

#[derive(Clone)]
pub struct StoreState {
    orders: Arc<RwLock<HashMap<i64, Order>>>,
    // Reference to pets for inventory calculation
    pets: Arc<RwLock<HashMap<i64, Animal>>>,
    next_id: Arc<RwLock<i64>>,
}

#[derive(Clone)]
pub struct UserState {
    users: Arc<RwLock<HashMap<String, User>>>,
}

// Pet service implementation - generic over any state that provides pet data
struct PetServiceImpl;

impl<S: PetStateProvider> PetService<S> for PetServiceImpl {
    async fn get_pet_by_id(
        State(state): State<S>,
        Path(pet_id): Path<i64>,
    ) -> Result<GetPetByIdResponse, GetPetByIdError> {
        let pets = state.pets().read().await;
        match pets.get(&pet_id) {
            Some(pet) => Ok(GetPetByIdResponse::Status200(pet.clone())),
            None => Err(GetPetByIdError::Status404),
        }
    }

    async fn add_pet(
        State(state): State<S>,
        Json(mut pet): Json<Animal>,
    ) -> Result<PetResponse, AddPetError> {
        let id = if let Some(id) = pet.id {
            id
        } else {
            let mut next_id = state.next_id().write().await;
            let id = *next_id;
            *next_id += 1;
            id
        };

        pet.id = Some(id);
        state.pets().write().await.insert(id, pet.clone());
        Ok(PetResponse::Created(pet))
    }

    async fn update_pet(
        State(state): State<S>,
        Json(pet): Json<Animal>,
    ) -> Result<UpdatePetResponse, UpdatePetError> {
        let id = pet.id.ok_or(UpdatePetError::Status400)?;
        let mut pets = state.pets().write().await;
        if !pets.contains_key(&id) {
            return Err(UpdatePetError::Status404);
        }
        pets.insert(id, pet.clone());
        Ok(UpdatePetResponse::Status200(pet))
    }

    async fn delete_pet(
        State(state): State<S>,
        Path(pet_id): Path<i64>,
    ) -> Result<DeletePetResponse, DeletePetError> {
        state.pets().write().await.remove(&pet_id);
        Ok(DeletePetResponse::Status200)
    }

    async fn find_pets_by_status(
        State(state): State<S>,
        Query(query): Query<FindPetsByStatusQuery>,
    ) -> Result<FindPetsByStatusResponse, FindPetsByStatusError> {
        let pets = state.pets().read().await;
        let filtered: Vec<Animal> =
            pets.values()
                .filter(|pet| {
                    query.status.as_ref().is_none_or(|s| {
                        pet.status.as_ref().map(|ps| ps.to_string()) == Some(s.clone())
                    })
                })
                .cloned()
                .collect();
        Ok(FindPetsByStatusResponse::Status200(filtered))
    }

    async fn find_pets_by_tags(
        State(state): State<S>,
        Query(query): Query<FindPetsByTagsQuery>,
    ) -> Result<FindPetsByTagsResponse, FindPetsByTagsError> {
        let pets = state.pets().read().await;
        let filtered: Vec<Animal> = pets
            .values()
            .filter(|pet| {
                query.tags.as_ref().is_none_or(|tags| {
                    tags.iter()
                        .any(|t| pet.tags.iter().any(|pt| pt.name.as_ref() == Some(t)))
                })
            })
            .cloned()
            .collect();
        Ok(FindPetsByTagsResponse::Status200(filtered))
    }

    async fn update_pet_with_form(
        State(state): State<S>,
        Path(pet_id): Path<i64>,
        Query(query): Query<UpdatePetWithFormQuery>,
    ) -> Result<UpdatePetWithFormResponse, UpdatePetWithFormError> {
        let mut pets = state.pets().write().await;
        if let Some(pet) = pets.get_mut(&pet_id) {
            if let Some(name) = query.name {
                pet.name = name;
            }
            if let Some(status) = query.status {
                pet.status = Some(match status.as_str() {
                    "available" => PetStatus::Available,
                    "pending" => PetStatus::Pending,
                    "sold" => PetStatus::Sold,
                    _ => return Err(UpdatePetWithFormError::Status400),
                });
            }
            Ok(UpdatePetWithFormResponse::Status200)
        } else {
            Err(UpdatePetWithFormError::Status400)
        }
    }

    async fn upload_file(
        State(_state): State<S>,
        Path(_pet_id): Path<i64>,
        Query(_query): Query<UploadFileQuery>,
    ) -> Result<UploadFileResponse, UploadFileError> {
        Ok(UploadFileResponse::Status200(ApiResponse {
            code: Some(200),
            type_: Some("success".to_string()),
            message: Some("File uploaded".to_string()),
        }))
    }
}

// Store service implementation
struct StoreServiceImpl;

impl StoreService for StoreServiceImpl {
    async fn get_inventory(
        State(state): State<StoreState>,
    ) -> Result<InventoryResponse, GetInventoryError> {
        let pets = state.pets.read().await;
        let mut _count_map = std::collections::HashMap::<String, i32>::new();
        for pet in pets.values() {
            if let Some(status) = &pet.status {
                *_count_map.entry(status.to_string()).or_default() += 1;
            }
        }
        // The Inventory type is generated from the inline schema
        Ok(InventoryResponse::Success(Inventory {}))
    }

    async fn place_order(
        State(state): State<StoreState>,
        Json(mut order): Json<Order>,
    ) -> Result<PlaceOrderResponse, PlaceOrderError> {
        let id = if let Some(id) = order.id {
            id
        } else {
            let mut next_id = state.next_id.write().await;
            let id = *next_id;
            *next_id += 1;
            id
        };
        order.id = Some(id);
        state.orders.write().await.insert(id, order.clone());
        Ok(PlaceOrderResponse::Status200(order))
    }

    async fn get_order_by_id(
        State(state): State<StoreState>,
        Path(order_id): Path<i64>,
    ) -> Result<GetOrderByIdResponse, GetOrderByIdError> {
        let orders = state.orders.read().await;
        match orders.get(&order_id) {
            Some(order) => Ok(GetOrderByIdResponse::Status200(order.clone())),
            None => Err(GetOrderByIdError::Status404),
        }
    }

    async fn delete_order(
        State(state): State<StoreState>,
        Path(order_id): Path<i64>,
    ) -> Result<DeleteOrderResponse, DeleteOrderError> {
        let mut orders = state.orders.write().await;
        if orders.remove(&order_id).is_some() {
            Ok(DeleteOrderResponse::Status200)
        } else {
            Err(DeleteOrderError::Status404)
        }
    }
}

// User service implementation
struct UserServiceImpl;

impl UserService for UserServiceImpl {
    async fn create_user(
        State(state): State<UserState>,
        Json(user): Json<User>,
    ) -> Result<CreateUserResponse, CreateUserError> {
        if let Some(username) = &user.username {
            state
                .users
                .write()
                .await
                .insert(username.clone(), user.clone());
            Ok(CreateUserResponse::Status200(user))
        } else {
            Err(CreateUserError::Default(
                axum::http::StatusCode::BAD_REQUEST,
                Error {
                    code: "missing_username".to_string(),
                    message: "Username is required".to_string(),
                },
            ))
        }
    }

    async fn create_users_with_list_input(
        State(state): State<UserState>,
        Json(users): Json<Vec<User>>,
    ) -> Result<CreateUsersWithListInputResponse, CreateUsersWithListInputError> {
        let mut store = state.users.write().await;
        let mut last_user = None;
        for user in users {
            if let Some(username) = &user.username {
                last_user = Some(user.clone());
                store.insert(username.clone(), user);
            }
        }
        Ok(CreateUsersWithListInputResponse::Status200(
            last_user.unwrap_or_default(),
        ))
    }

    async fn login_user(
        State(state): State<UserState>,
        Query(query): Query<LoginUserQuery>,
    ) -> Result<LoginUserResponse, LoginUserError> {
        let users = state.users.read().await;
        if let (Some(username), Some(_password)) = (&query.username, &query.password) {
            if users.contains_key(username) {
                Ok(LoginUserResponse::Status200(
                    "session-token-12345".to_string(),
                ))
            } else {
                Err(LoginUserError::Status400)
            }
        } else {
            Err(LoginUserError::Status400)
        }
    }

    async fn logout_user(
        State(_state): State<UserState>,
    ) -> Result<LogoutUserResponse, LogoutUserError> {
        Ok(LogoutUserResponse::Status200)
    }

    async fn get_user_by_name(
        State(state): State<UserState>,
        Path(username): Path<String>,
    ) -> Result<GetUserByNameResponse, GetUserByNameError> {
        let users = state.users.read().await;
        match users.get(&username) {
            Some(user) => Ok(GetUserByNameResponse::Status200(user.clone())),
            None => Err(GetUserByNameError::Status404),
        }
    }

    async fn update_user(
        State(state): State<UserState>,
        Path(username): Path<String>,
        Json(user): Json<User>,
    ) -> Result<UpdateUserResponse, UpdateUserError> {
        let mut users = state.users.write().await;
        users.insert(username, user);
        Ok(UpdateUserResponse::Status200)
    }

    async fn delete_user(
        State(state): State<UserState>,
        Path(username): Path<String>,
    ) -> Result<DeleteUserResponse, DeleteUserError> {
        let mut users = state.users.write().await;
        if users.remove(&username).is_some() {
            Ok(DeleteUserResponse::Status200)
        } else {
            Err(DeleteUserError::Status404)
        }
    }
}

#[tokio::main]
async fn main() {
    // Create separate state for each service
    let pets = Arc::new(RwLock::new(HashMap::new()));

    let pet_state = PetState {
        pets: pets.clone(),
        next_id: Arc::new(RwLock::new(3)),
    };

    let store_state = StoreState {
        orders: Arc::new(RwLock::new(HashMap::new())),
        pets: pets.clone(), // Share pets for inventory
        next_id: Arc::new(RwLock::new(1)),
    };

    let user_state = UserState {
        users: Arc::new(RwLock::new(HashMap::new())),
    };

    // Seed some pet data
    {
        let mut pets = pet_state.pets.write().await;
        pets.insert(
            1,
            Animal {
                id: Some(1),
                name: "Doggie".to_string(),
                photo_urls: vec!["https://example.com/doggie.jpg".to_string()],
                category: Some(Category {
                    id: Some(1),
                    name: Some("Dogs".to_string()),
                }),
                tags: vec![Tag {
                    id: Some(1),
                    name: Some("friendly".to_string()),
                }],
                status: Some(PetStatus::Available),
            },
        );
        pets.insert(
            2,
            Animal {
                id: Some(2),
                name: "Cat".to_string(),
                photo_urls: vec!["https://example.com/cat.jpg".to_string()],
                category: Some(Category {
                    id: Some(2),
                    name: Some("Cats".to_string()),
                }),
                tags: vec![],
                status: Some(PetStatus::Pending),
            },
        );
    }

    // Seed a user
    {
        let mut users = user_state.users.write().await;
        users.insert(
            "testuser".to_string(),
            User {
                id: Some(1),
                username: Some("testuser".to_string()),
                first_name: Some("Test".to_string()),
                last_name: Some("User".to_string()),
                email: Some("test@example.com".to_string()),
                password: Some("password123".to_string()),
                phone: Some("555-1234".to_string()),
                user_status: Some(1),
            },
        );
    }

    // Compose routers from each service with their own state
    let app = Router::new()
        .merge(PetServiceImpl::map_routes(Router::new()).with_state(pet_state))
        .merge(StoreServiceImpl::map_routes(Router::new()).with_state(store_state))
        .merge(UserServiceImpl::map_routes(Router::new()).with_state(user_state));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    println!("Petstore API running on http://localhost:3000");
    println!();
    println!("Services:");
    println!("  - PetService (PetState)");
    println!("  - StoreService (StoreState)");
    println!("  - UserService (UserState)");
    println!();
    println!("Try:");
    println!("  curl http://localhost:3000/pet/1");
    println!("  curl http://localhost:3000/pet/findByStatus?status=available");
    println!("  curl http://localhost:3000/store/inventory");
    println!("  curl http://localhost:3000/user/testuser");
    println!("  curl -X POST http://localhost:3000/pet -H 'Content-Type: application/json' \\");
    println!("       -d '{{\"name\":\"Bird\",\"photoUrls\":[]}}'");

    axum::serve(listener, app).await.unwrap();
}
