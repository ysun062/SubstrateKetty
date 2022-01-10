#![cfg_attr(not(feature = "std"), no_std)]

/// A module for proof of existence
pub use pallet::*;

#[cfg(test)]
mod mock;
#[cfg(test)]
mod tests;

#[frame_support::pallet]
pub mod pallet {
    use frame_support::pallet_prelude::*;
	use frame_system::pallet_prelude::*;


    use frame_support::{
        dispatch::DispatchResultWithPostInfo,
        pallet_prelude::*,
		sp_runtime::traits::{AtLeast32BitUnsigned, Bounded, Hash},
		traits::{ Randomness, Currency, tokens::ExistenceRequirement, ReservableCurrency },
		transactional
	};

    use sp_io::hashing::blake2_128;
	use scale_info::TypeInfo;


    #[cfg(feature = "std")]
	use frame_support::serde::{Deserialize, Serialize};

    type AccountOf<T> = <T as frame_system::Config>::AccountId;
    type BalanceOf<T> = <<T as Config>::Currency as Currency<<T as frame_system::Config>::AccountId>>::Balance;


    // Struct for holding Kitty information.
	#[derive(Clone, Encode, Decode, PartialEq, RuntimeDebug, TypeInfo)]
	#[scale_info(skip_type_params(T))]
	pub struct Kitty<T: Config> {
		pub dna: [u8; 16],   // Using 16 bytes to represent a kitty DNA
		pub price: Option<BalanceOf<T>>,
		pub owner: AccountOf<T>,
	}


    #[pallet::pallet]
    #[pallet::generate_store(pub(super) trait Store)]
    pub struct Pallet<T>(_);

    #[pallet::config]
    pub trait Config: frame_system::Config {
		/// Because this pallet emits events, it depends on the runtime's definition of an event.
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

		/// The Currency handler for the Kitties pallet.
		//type Currency: Currency<Self::AccountId>;
		type Currency: Currency<Self::AccountId> + ReservableCurrency<Self::AccountId>;

		/// The maximum amount of Kitties a single account can own.
		#[pallet::constant]
		type MaxKittyOwned: Get<u32>;

		/// The type of Randomness we want to specify for this pallet.
		type KittyRandomness: Randomness<Self::Hash, Self::BlockNumber>;

		#[pallet::constant]
		type StakeForEachKitty: Get<BalanceOf<Self>>;
		type KittyIndex: Parameter + AtLeast32BitUnsigned + Default + Copy + Bounded;
    }


    // Storage items.

	#[pallet::storage]
	#[pallet::getter(fn kitty_cnt)]
	/// Keeps track of the number of Kitties in existence.
	pub(super) type KittyCnt<T: Config> = StorageValue<_, u64, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn kitties)]
	/// Stores a Kitty's unique traits, owner and price.
	pub(super) type Kitties<T: Config> = StorageMap<_, Twox64Concat, T::Hash, Kitty<T>>;

	#[pallet::storage]
	#[pallet::getter(fn kitties_owned)]
	/// Keeps track of what accounts own what Kitty.
	pub(super) type KittiesOwned<T: Config> =
		StorageMap<_, Twox64Concat, T::AccountId, BoundedVec<T::Hash, T::MaxKittyOwned>, ValueQuery>;

	// Our pallet's genesis configuration.
	#[pallet::genesis_config]
	pub struct GenesisConfig<T: Config> {
		pub kitties: Vec<(T::AccountId, [u8; 16])>,
	}

	// Required to implement default for GenesisConfig.
	#[cfg(feature = "std")]
	impl<T: Config> Default for GenesisConfig<T> {
		fn default() -> GenesisConfig<T> {
			GenesisConfig { kitties: vec![] }
		}
	}

	#[pallet::genesis_build]
	impl<T: Config> GenesisBuild<T> for GenesisConfig<T> {
		fn build(&self) {
			// When building a kitty from genesis config, we require the dna and gender to be supplied.
			for (acct, dna) in &self.kitties {
				let _ = <Pallet<T>>::mint(acct, Some(dna.clone()));
			}
		}
	}

	#[pallet::hooks]
	impl<T: Config> Hooks<BlockNumberFor<T>> for Pallet<T> {}

    #[pallet::event]
    #[pallet::generate_deposit(pub(super) fn deposit_event)]
    pub enum Event<T: Config>{
		/// A new Kitty was sucessfully created. \[sender, kitty_id\]
		Created(T::AccountId, T::Hash),
		/// Kitty price was sucessfully set. \[sender, kitty_id, new_price\]
		PriceSet(T::AccountId, T::Hash, Option<BalanceOf<T>>),
		/// A Kitty was sucessfully transferred. \[from, to, kitty_id\]
		Transferred(T::AccountId, T::AccountId, T::Hash),
		/// A Kitty was sucessfully bought. \[buyer, seller, kitty_id, bid_price\]
		Bought(T::AccountId, T::AccountId, T::Hash, BalanceOf<T>),
    }

    #[pallet::error]
    pub enum Error<T> {
		/// Handles arithemtic overflow when incrementing the Kitty counter.
		KittyCntOverflow,
		/// An account cannot own more Kitties than `MaxKittyCount`.
		ExceedMaxKittyOwned,
		/// Buyer cannot be the owner.
		BuyerIsKittyOwner,
		/// Cannot transfer a kitty to its owner.
		TransferToSelf,
		/// Handles checking whether the Kitty exists.
		KittyNotExist,
		/// Handles checking that the Kitty is owned by the account transferring, buying or setting a price for it.
		NotKittyOwner,
		/// Ensures the Kitty is for sale.
		KittyNotForSale,
		/// Ensures that the buying price is greater than the asking price.
		KittyBidPriceTooLow,
		/// Ensures that an account has enough funds to purchase a Kitty. 
		NotEnoughBalance,
		/// Ensure that an account has enough funds for staking.
		NotEnoughBalanceForStaking,
    }

    #[pallet::call]
    impl<T: Config> Pallet<T> {

        /// Create a new unique kitty.
		///
		/// The actual kitty creation is done in the `mint()` function.
		#[pallet::weight(100)]
		pub fn create_kitty(origin: OriginFor<T>) -> DispatchResult {
			let sender = ensure_signed(origin)?;

			let stake_amount = T::StakeForEachKitty::get();
			// set staking reserve for owner
			T::Currency::reserve(&sender, stake_amount)
				.map_err(|_| Error::<T>::NotEnoughBalanceForStaking)?;

			let kitty_id = Self::mint(&sender, None)?;

			// Logging to the console
			log::info!("A kitty is born with ID ➡ {:?}.", kitty_id);
			// Deposit our "Created" event.
			Self::deposit_event(Event::Created(sender, kitty_id));
			Ok(())
		}


        /// Directly transfer a kitty to another recipient.
		///
		/// Any account that holds a kitty can send it to another Account. This will reset the asking
		/// price of the kitty, marking it not for sale.
		#[pallet::weight(100)]
		pub fn transfer(
			origin: OriginFor<T>, 
			to: T::AccountId, 
			kitty_id: T::Hash
		) -> DispatchResult {
			let from = ensure_signed(origin)?;

			// Ensure the kitty exists and is called by the kitty owner
			ensure!(Self::is_kitty_owner(&kitty_id, &from)?, <Error<T>>::NotKittyOwner);

			// Verify the kitty is not transferring back to its owner.
			ensure!(from != to, <Error<T>>::TransferToSelf);

			// Verify the recipient has the capacity to receive one more kitty
			let to_owned = <KittiesOwned<T>>::get(&to);
			ensure!((to_owned.len() as u32) < T::MaxKittyOwned::get(), <Error<T>>::ExceedMaxKittyOwned);

			//staking
			let stake_amount = T::StakeForEachKitty::get();
			T::Currency::reserve(&to, stake_amount)
				.map_err(|_| Error::<T>::NotEnoughBalanceForStaking)?;

			//Unreserve the original owner
			T::Currency::unreserve(&from, stake_amount);

			Self::transfer_kitty_to(&kitty_id, &to)?;

			Self::deposit_event(Event::Transferred(from, to, kitty_id));

			Ok(())
		}

        /// Breed a Kitty.
		///
		/// Breed two kitties to create a new generation
		/// of Kitties.
		#[pallet::weight(100)]
		pub fn breed_kitty(
			origin: OriginFor<T>, 
			kid1: T::Hash, 
			kid2: T::Hash
		) -> DispatchResult {
			let sender = ensure_signed(origin)?;

			// Check: Verify `sender` owns both kitties (and both kitties exist).
			ensure!(Self::is_kitty_owner(&kid1, &sender)?, <Error<T>>::NotKittyOwner);
			ensure!(Self::is_kitty_owner(&kid2, &sender)?, <Error<T>>::NotKittyOwner);

			let new_dna = Self::breed_dna(&kid1, &kid2)?;
			Self::mint(&sender, Some(new_dna))?;

			Ok(())
		}


        #[transactional]
		#[pallet::weight(100)]
        pub fn buy_kitty(origin: OriginFor<T>, kitty_id: T::Hash, bid_price: BalanceOf<T>) -> DispatchResult {
            let buyer = ensure_signed(origin)?;
            // check the kitty exists, and the buyer is not the current kitty owner
            let kitty = Self::kitties(kitty_id).ok_or(<Error<T>>::KittyNotExist)?;
            ensure!(kitty.owner != buyer, <Error<T>>::BuyerIsKittyOwner);

            //check the kitty is for sale & that the buying price is greater than the selling price
            if let Some(ask_price) = kitty.price {
                ensure!(ask_price <= bid_price, <Error<T>>::KittyBidPriceTooLow);
            }
            else {
                Err(<Error<T>>::KittyNotForSale)?;
            }

            //check that the owner has enough money in the balance to buy
            ensure!(T::Currency::free_balance(&buyer) >= bid_price, <Error<T>>::NotEnoughBalance);

            let seller = kitty.owner.clone();

			let stake_amount = T::StakeForEachKitty::get();

			//set buyer's reserve to staking amount
			T::Currency::reserve(&buyer, stake_amount)
			.map_err(|_| Error::<T>::NotEnoughBalanceForStaking)?;

			//unreserve the seller
			T::Currency::unreserve(&seller, stake_amount);

            //Transfer the money from buyer to the seller
            T::Currency::transfer(&buyer, &seller, bid_price, ExistenceRequirement::KeepAlive)?;

            //Transfer the kitty from seller to buyer
            Self::transfer_kitty_to(&kitty_id, &buyer)?;

            Self::deposit_event(Event::Bought(buyer, seller, kitty_id, bid_price));

            Ok(())
        }

        /// Set the price for a Kitty.
		///
		/// Updates Kitty price and updates storage.
		#[pallet::weight(100)]
		pub fn set_price(
			origin: OriginFor<T>, 
			kitty_id: T::Hash, 
			new_price: Option<BalanceOf<T>>
		) -> DispatchResult {
			let sender = ensure_signed(origin)?;

			// Ensure the kitty exists and is called by the kitty owner
			ensure!(Self::is_kitty_owner(&kitty_id, &sender)?, <Error<T>>::NotKittyOwner);

			let mut kitty = Self::kitties(&kitty_id).ok_or(<Error<T>>::KittyNotExist)?;

			kitty.price = new_price.clone();
			<Kitties<T>>::insert(&kitty_id, kitty);

			// Deposit a "PriceSet" event.
			Self::deposit_event(Event::PriceSet(sender, kitty_id, new_price));

			Ok(())
		}
    }

    impl<T: Config> Pallet<T> {

        fn transfer_kitty_to(kitty_id: &T::Hash, to: &T::AccountId) -> Result<(), Error<T>> {
            let mut kitty = Self::kitties(kitty_id).ok_or(<Error<T>>::KittyNotExist)?;

            let prev_owner = kitty.owner.clone();

            //Remove "kitty_id" from the KittyOwned Vec of prev_owner
            <KittiesOwned<T>>::try_mutate(&prev_owner, |owned|{
                if let Some(ind) = owned.iter().position(|&id| id == *kitty_id){
                    owned.swap_remove(ind);
                    return Ok(());
                }
                Err(())
            }).map_err(|_| <Error<T>>::KittyNotExist)?;

            //Update the kitty owner
            kitty.owner = to.clone();
            //make kitty not for sale
            kitty.price = None;

            <Kitties<T>>::insert(kitty_id, kitty);

            <KittiesOwned<T>>::try_mutate(to, |vec| {
                vec.try_push(*kitty_id)
            }).map_err(|_| <Error<T>>::ExceedMaxKittyOwned)?;

            Ok(())


        }

        fn gen_dna() -> [u8; 16] {
			let payload = (
				T::KittyRandomness::random(&b"dna"[..]).0,
				<frame_system::Pallet<T>>::block_number(),
			);
			payload.using_encoded(blake2_128)
		}

        pub fn breed_dna(kid1: &T::Hash, kid2: &T::Hash) -> Result<[u8; 16], Error<T>> {
			let dna1 = Self::kitties(kid1).ok_or(<Error<T>>::KittyNotExist)?.dna;
			let dna2 = Self::kitties(kid2).ok_or(<Error<T>>::KittyNotExist)?.dna;

			let mut new_dna = Self::gen_dna();
			for i in 0..new_dna.len() {
				new_dna[i] = (new_dna[i] & dna1[i]) | (!new_dna[i] & dna2[i]);
			}
			Ok(new_dna)
		}

        // Helper to mint a Kitty.
		pub fn mint(
			owner: &T::AccountId,
			dna: Option<[u8; 16]>,
		) -> Result<T::Hash, Error<T>> {
			let kitty = Kitty::<T> {
				dna: dna.unwrap_or_else(Self::gen_dna),
				price: None,
				owner: owner.clone(),
			};

			let kitty_id = T::Hashing::hash_of(&kitty);

			// Performs this operation first as it may fail
			let new_cnt = Self::kitty_cnt().checked_add(1)
				.ok_or(<Error<T>>::KittyCntOverflow)?;

			// Performs this operation first because as it may fail
			<KittiesOwned<T>>::try_mutate(&owner, |kitty_vec| {
				kitty_vec.try_push(kitty_id)
			}).map_err(|_| <Error<T>>::ExceedMaxKittyOwned)?;

			<Kitties<T>>::insert(kitty_id, kitty);
			<KittyCnt<T>>::put(new_cnt);
			Ok(kitty_id)
		}

        pub fn is_kitty_owner(kitty_id: &T::Hash, acct: &T::AccountId) -> Result<bool, Error<T>> {
			match Self::kitties(kitty_id) {
				Some(kitty) => Ok(kitty.owner == *acct),
				None => Err(<Error<T>>::KittyNotExist)
			}
		}
    }
    
}
