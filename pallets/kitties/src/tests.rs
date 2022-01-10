// #![cfg(test)]

// use crate::{
// 	mock::*, pallet::{Error, Event}
// };
// use frame_support::{assert_ok, assert_noop};
// use super::*;

use crate::{Error, Event, mock::*};
use frame_support::{assert_ok, assert_noop};
use super::*;

#[test]
fn should_build_genesis_kitties() {
	new_test_ext().execute_with(|| {
		// Check we have 2 kitties, as specified
		assert_eq!(SubstrateKitties::kitty_cnt(), 2);

		// Check owners own the correct amount of kitties
		let kitties_owned_by_1 = SubstrateKitties::kitties_owned(1);
		assert_eq!(kitties_owned_by_1.len(), 1);

		let kitties_owned_by_2 = SubstrateKitties::kitties_owned(2);
		assert_eq!(kitties_owned_by_2.len(), 1);

		// Check that kitties are owned correctly
		let kid1 = kitties_owned_by_1[0];
		let kitty1 = SubstrateKitties::kitties(kid1)
			.expect("Could have this kitty ID owned by acct 1");
		assert_eq!(kitty1.owner, 1);

		let kid2 = kitties_owned_by_2[0];
		let kitty2 = SubstrateKitties::kitties(kid2)
			.expect("Could have this kitty ID owned by acct 2");
		assert_eq!(kitty2.owner, 2);
	});
}

#[test]
fn create_kitty_should_work() {
	new_test_ext().execute_with(|| {
		// create a kitty with account #10.
		assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));

		// check that 3 kitties exists (together with the two from genesis)
		assert_eq!(SubstrateKitties::kitty_cnt(), 3);

		// check that account #10 owns 1 kitty
		assert_eq!(SubstrateKitties::kitties_owned(10).len(), 1);

		// check that some random account #5 does not own a kitty
		assert_eq!(SubstrateKitties::kitties_owned(5).len(), 0);

		// check that this kitty is specifically owned by account #10
		let hash = SubstrateKitties::kitties_owned(10)[0];
		let kitty = SubstrateKitties::kitties(hash).expect("should found the kitty");
		assert_eq!(kitty.owner, 10);
		assert_eq!(kitty.price, None);
	});
}

#[test]
fn create_kitty_failed_Exceed_Max_Kitty_Owned() {
	new_test_ext().execute_with(|| {
        assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));
        assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));
        assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));

        assert_noop!(
			SubstrateKitties::create_kitty(Origin::signed(10)),
			Error::<Test>::ExceedMaxKittyOwned
		);
	});
}



#[test]
fn transfer_kitty_should_work() {
	new_test_ext().execute_with(|| {
		// check that acct 10 own a kitty
		assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));
		assert_eq!(SubstrateKitties::kitties_owned(10).len(), 1);
		let hash = SubstrateKitties::kitties_owned(10)[0];

		// acct 10 send kitty to acct 3
		assert_ok!(SubstrateKitties::transfer(Origin::signed(10), 3, hash));

		// acct 10 now has nothing
		assert_eq!(SubstrateKitties::kitties_owned(10).len(), 0);
		// but acct 3 does
		assert_eq!(SubstrateKitties::kitties_owned(3).len(), 1);
		let new_hash = SubstrateKitties::kitties_owned(3)[0];
		// and it has the same hash
		assert_eq!(hash, new_hash);
	});
}

#[test]
fn transfer_non_owned_kitty_should_fail() {
	new_test_ext().execute_with(|| {
		let hash = SubstrateKitties::kitties_owned(1)[0];

		// account 0 cannot transfer a kitty with this hash.
		assert_noop!(
			SubstrateKitties::transfer(Origin::signed(9), 2, hash),
			Error::<Test>::NotKittyOwner
		);
	});
}


#[test]
fn breed_kitty_should_work() {
	new_test_ext().execute_with(|| {
		assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));
        assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));

		let hashKitty1 = SubstrateKitties::kitties_owned(10)[0];
		let kitty1 = SubstrateKitties::kitties(hashKitty1).expect("should found the kitty");

		let hashKitty2 = SubstrateKitties::kitties_owned(10)[1];
		let kitty2 = SubstrateKitties::kitties(hashKitty2).expect("should found the kitty");

		assert_ok!(SubstrateKitties::breed_kitty(Origin::signed(10), hashKitty1, hashKitty2));

		//Check out that the account should have 3 kitties
		let kitties_owned_by_10 = SubstrateKitties::kitties_owned(10);
		assert_eq!(kitties_owned_by_10.len(), 3);
	});

}

#[test]
fn breed_kitty_failed_Not_Kitty_Owner() {
	new_test_ext().execute_with(|| {
		assert_ok!(SubstrateKitties::create_kitty(Origin::signed(10)));
        assert_ok!(SubstrateKitties::create_kitty(Origin::signed(11)));

		let hashKitty1 = SubstrateKitties::kitties_owned(10)[0];
		let kitty1 = SubstrateKitties::kitties(hashKitty1).expect("should found the kitty");

		let hashKitty2 = SubstrateKitties::kitties_owned(11)[0];
		let kitty2 = SubstrateKitties::kitties(hashKitty2).expect("should found the kitty");

		assert_noop!(
			SubstrateKitties::breed_kitty(Origin::signed(10), hashKitty1, hashKitty2),
			Error::<Test>::NotKittyOwner
		);
	});
}

//buy_kitty
#[test]
fn buy_kitty_works() {
	new_test_ext().execute_with(|| {


		let hashKitty1 = SubstrateKitties::kitties_owned(1)[0];
		let kitty1 = SubstrateKitties::kitties(hashKitty1).expect("should found the kitty");

		let hashKitty2 = SubstrateKitties::kitties_owned(2)[0];
		let kitty2 = SubstrateKitties::kitties(hashKitty2).expect("should found the kitty");

		let price: u64 = 3;

		assert_ok!(SubstrateKitties::set_price(Origin::signed(2), hashKitty2, Some(price)));
		assert_ok!(SubstrateKitties::buy_kitty(Origin::signed(1), hashKitty2, 3));
	});
}

//buy_kitty
#[test]
fn buy_kitty_failed_not_kitty_owner() {
	new_test_ext().execute_with(|| {


		let hashKitty1 = SubstrateKitties::kitties_owned(1)[0];
		let kitty1 = SubstrateKitties::kitties(hashKitty1).expect("should found the kitty");

		let hashKitty2 = SubstrateKitties::kitties_owned(2)[0];
		let kitty2 = SubstrateKitties::kitties(hashKitty2).expect("should found the kitty");

		let price: u64 = 4;

		assert_ok!(SubstrateKitties::set_price(Origin::signed(2), hashKitty2, Some(price)));

		assert_noop!(
			SubstrateKitties::buy_kitty(Origin::signed(1), hashKitty2, 3),
			Error::<Test>::KittyBidPriceTooLow
		);
	});
}

//set_price(sell)_kitty
#[test]
fn set_price_kitty_works() {
	new_test_ext().execute_with(|| {

		let hashKitty1 = SubstrateKitties::kitties_owned(1)[0];
		let kitty1 = SubstrateKitties::kitties(hashKitty1).expect("should found the kitty");

		let hashKitty2 = SubstrateKitties::kitties_owned(2)[0];
		let kitty2 = SubstrateKitties::kitties(hashKitty2).expect("should found the kitty");

		let price: u64 = 4;
		assert_ok!(SubstrateKitties::set_price(Origin::signed(2), hashKitty2, Some(price)));
	});
}

//set_price(sell)_kitty
#[test]
fn set_price_kitty_failed_not_kitty_owner() {
	new_test_ext().execute_with(|| {

		let hashKitty1 = SubstrateKitties::kitties_owned(1)[0];
		let kitty1 = SubstrateKitties::kitties(hashKitty1).expect("should found the kitty");

		let hashKitty2 = SubstrateKitties::kitties_owned(2)[0];
		let kitty2 = SubstrateKitties::kitties(hashKitty2).expect("should found the kitty");

		let price: u64 = 4;

		assert_noop!(
			SubstrateKitties::set_price(Origin::signed(1), hashKitty2, Some(price)),
			Error::<Test>::NotKittyOwner
		);
	});
}


